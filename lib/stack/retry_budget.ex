defmodule Stack.RetryBudget do
  @moduledoc """
  Budget that limits the number of retries based on the number of requests.

  The retry budget allows a minimum number of retries per second. It also allows
  retries up to a percentage of requests.
  """

  alias Stack.{Budget, RetryBudget}
  require Record
  use Bitwise
  use GenServer

  @behaviour Budget

  @allow 20
  @min 10
  @interval 1_000

  Record.defrecord(:requests, count: 0)
  Record.defrecord(:retries, first: 0, second: 0, rest: 0)

  defmodule Update do
    @moduledoc false
    @enforce_keys [:time, :tid, :allow, :min, :interval]
    defstruct [:time, :tid, :allow, :min, :interval, delay: 0, count_ema: 0]
  end

  @typedoc """
  Options for retry budget.

    * `:name` - The registered name of the retry budget (required).
    * `:allow` - The number of allowed retries as a percentage (integer) of requests (default: `20`)
    * `:min` - The minimum number of retries to allow in an interval (default: `10`)
    * `interval` - The time interval (in milliseconds) to track requests count and minimum retries
    (default: `1000`)
  """
  @type option ::
          {:name, atom}
          | {:allow, non_neg_integer}
          | {:min, non_neg_integer}
          | {:interval, pos_integer}

  @doc """
  Start a retry budget configured with a keyword list of options.
  """
  @spec start_link([option]) :: {:ok, pid} | {:error, term}
  def start_link(opts) do
    GenServer.start_link(RetryBudget, opts, name: Keyword.take(opts, :name))
  end

  @impl Budget
  def ask(name) do
    force(name)
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
  def ack(_tid), do: :ok

  @impl Budget
  def nack(tid, 1) do
    tokens = :ets.update_counter(tid, :retries, {retries(:first) + 1, -1})
    tokens >= 0 or nack(tid, 2)
  end

  def nack(tid, 2) do
    tokens = :ets.update_counter(tid, :retries, {retries(:second) + 1, -1})
    tokens >= 0 or nack(tid, 3)
  end

  def nack(tid, _) do
    tokens = :ets.update_counter(tid, :retries, {retries(:rest) + 1, -1})
    tokens >= 0
  end

  @impl GenServer
  def init(opts) do
    name = Keyword.fetch!(opts, :name)
    allow = non_neg_integer(opts, :allow, @allow)
    min = non_neg_integer(opts, :min, @min)
    interval = pos_integer(opts, :interval, @interval)
    ^name = :ets.new(name, [:named_table, :public, {:write_concurrency, true}])
    tid = :ets.whereis(name)
    true = :ets.insert_new(tid, [requests(), new_retries(0, min)])
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
      count_ema: count_ema
    } = update

    get_reset = [{requests(:count) + 1, 0}, {requests(:count) + 1, 0, 0, 0}]
    [count | _] = :ets.update_counter(tid, :requests, get_reset)
    # EMA is weighted based on the time delay since last update, larger delay means more weight
    count_ema = ema(delay, count, interval, count_ema)

    delay2 = next(%Update{update | count_ema: count_ema})
    tokens = div(count_ema * allow * delay2, 100 * interval)
    retries = new_retries(tokens, min)
    true = :ets.insert(tid, retries)
    {:noreply, tid}
  end

  ## internal

  defp non_neg_integer(opts, key, default) do
    integer(opts, key, default, 0)
  end

  defp pos_integer(opts, key, default) do
    integer(opts, key, default, 1)
  end

  defp integer(opts, key, default, min) do
    case Keyword.get(opts, key, default) do
      int when is_integer(int) and int >= min ->
        int

      val ->
        raise ArgumentError,
              "expected integer >= #{inspect(min)} for #{inspect(key)}, got: #{inspect(val)}"
    end
  end

  defp next(%Update{time: now, interval: interval} = update) do
    # randomize the interval to prevent synchronization between different budgets
    delay = div(interval, 2) + :rand.uniform(interval)
    next = now + delay
    _ = Process.send_after(self(), %Update{update | time: next, delay: delay}, next, abs: true)
    delay
  end

  defp new_retries(tokens, min) do
    # 1/2 tokens reserved for at least first try, 1/3 for second try, 1/6 for third or more
    # to prevent a small number of requests that always fail taking all tokens
    first = div(tokens, 2)
    rest = div(first, 3)
    second = tokens - first - rest
    retries(first: first, second: second, rest: min + rest)
  end

  defp ema(weight1, val1, weight2, val2) do
    div(weight1 * val1 + weight2 * val2, 2 * (weight1 + weight2))
  end
end
