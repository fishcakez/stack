defmodule Stack.RejectBudget do
  @moduledoc """
  Budget that limits the number of requests based on number of rejections.

  The reject budget allows at minimum number of requests per second. It also allows
  requests up to a percentage of rejections, then randomly drops requests to try to
  stabilize the number of accepted requests.
  """

  alias Stack.{Budget, RetryBudget}
  require Record
  use Bitwise
  use GenServer

  @behaviour Budget

  @allow 50
  @min 10
  @interval 1_000
  @scale 1_000_000

  Record.defrecord(:requests, count: 0, min: 0, drop_scaled: 0)
  Record.defrecord(:replies, accept: 0, reject: 0)

  defmodule Update do
    @moduledoc false
    @enforce_keys [:time, :tid, :allow, :min, :interval]
    defstruct [:time, :tid, :allow, :min, :interval, delay: 0, accept_ema: 0, reject_ema: 0]
  end

  @typedoc """
  Options for retry budget.

    * `:name` - The registered name of the reject budget (required).
    * `:allow` - The number of allowed rejections as a percentage (integer) of requests (default: `50`)
    * `:min` - The minimum number of requests to allow in an interval (default: `10`)
    * `interval` - The time interval (in milliseconds) to track rejection count and minimum requests
    (default: `1000`)
  """
  @type option ::
          {:name, atom}
          | {:allow, 0..100}
          | {:min, non_neg_integer}
          | {:interval, pos_integer}

  @doc """
  Start a reject budget configured with a keyword list of options.
  """
  @spec start_link([option]) :: {:ok, pid} | {:error, term}
  def start_link(opts) do
    GenServer.start_link(RetryBudget, opts, name: Keyword.take(opts, :name))
  end

  @impl Budget
  def ask(name) do
    case :ets.whereis(name) do
      :undefined ->
        raise ArgumentError, "#{inspect(RejectBudget)} #{name} is not started"

      tid ->
        maybe_drop(tid)
    end
  end

  @impl Budget
  def force(name) do
    case :ets.whereis(name) do
      :undefined ->
        raise ArgumentError, "#{inspect(RejectBudget)} #{name} is not started"

      tid ->
        _ = :ets.update_counter(tid, :requests, {requests(:count) + 1, 1})
        {:go, tid}
    end
  end

  @impl Budget
  def ack(tid) do
    _ = :ets.update_counter(tid, :requests, {replies(:accept) + 1, 1})
    :ok
  end

  @impl Budget
  def nack(tid, _) do
    _ = :ets.update_counter(tid, :requests, {replies(:reject) + 1, 1})
    true
  end

  @impl GenServer
  def init(opts) do
    name = Keyword.fetch!(opts, :name)
    allow = integer(opts, :allow, @allow, 0, 100)
    min = non_neg_integer(opts, :min, @min)
    interval = pos_integer(opts, :interval, @interval)
    ^name = :ets.new(name, [:named_table, :public, {:write_concurrency, true}])
    tid = :ets.whereis(name)
    true = :ets.insert_new(tid, [requests(min: min), replies()])
    now = System.monotonic_time(:millisecond)
    _ = next(%Update{time: now, tid: tid, allow: allow, min: min, interval: interval})
    {:ok, tid}
  end

  @impl GenServer
  def handle_info(update, tid) do
    %Update{
      tid: ^tid,
      allow: allow,
      min: min,
      interval: interval,
      delay: delay,
      accept_ema: accept_ema,
      reject_ema: reject_ema
    } = update

    get_reset = [
      {replies(:accept) + 1, 0},
      {replies(:reject) + 1, 0},
      {replies(:accept) + 1, 0, 0, 0},
      {replies(:reject) + 1, 0, 0, 0}
    ]

    [accept, reject | _] = :ets.update_counter(tid, :requests, get_reset)
    # EMA is weighted based on the time delay since last update, larger delay means more weight
    accept_ema = ema(delay, accept, interval, accept_ema)
    reject_ema = ema(delay, reject, interval, reject_ema)

    drop_scaled = drop_scaled(accept_ema, reject_ema, allow)
    true = :ets.insert(tid, requests(min: min, drop_scaled: drop_scaled))
    _ = next(%Update{update | accept_ema: accept_ema, reject_ema: reject_ema})
    {:noreply, tid}
  end

  ## internal

  defp non_neg_integer(opts, key, default) do
    integer(opts, key, default, 0, :infinity)
  end

  defp pos_integer(opts, key, default) do
    integer(opts, key, default, 1, :infinity)
  end

  defp integer(opts, key, default, min, max) do
    case Keyword.get(opts, key, default) do
      int when is_integer(int) and int >= min and int <= max ->
        int

      val ->
        raise ArgumentError,
              "expected #{min} <= integer <= #{max} for #{inspect(key)}, got: #{inspect(val)}"
    end
  end

  defp next(%Update{time: now, interval: interval} = update) do
    # randomize the interval to prevent synchronization between different budgets
    delay = div(interval, 2) + :rand.uniform(interval)
    next = now + delay
    _ = Process.send_after(self(), %Update{update | time: next, delay: delay}, next, abs: true)
    delay
  end

  defp maybe_drop(tid) do
    incr_get = [
      {requests(:count) + 1, 1},
      {requests(:min) + 1, 0},
      {requests(:drop_scaled) + 1, 0}
    ]

    case :ets.update_counter(tid, :requests, incr_get) do
      [count, min, drop_scaled] when count > min and drop_scaled > 0 ->
        maybe_drop(tid, drop_scaled)

      _ ->
        {:go, tid}
    end
  end

  defp maybe_drop(tid, drop_scaled) do
    case :rand.uniform(@scale) do
      n when n <= drop_scaled ->
        :drop

      _ ->
        {:go, tid}
    end
  end

  defp ema(weight1, val1, weight2, val2) do
    div(weight1 * val1 + weight2 * val2, 2 * (weight1 + weight2))
  end

  defp drop_scaled(_, _, 100), do: 0

  defp drop_scaled(accepts, rejects, allow) do
    requests = accepts + rejects
    weight = div(@scale * 100, 100 - allow)
    max(0, div(@scale * requests - weight * accepts, requests + 1))
  end
end
