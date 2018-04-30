defmodule Stack.Retry do
  @moduledoc """
  Retry handling with Stack.Filter.

  This module provides retry handling via a Stack.Filter.
  """
  alias Stack.{Retry, Context, Deadline, Filter}
  use Bitwise
  @behaviour Filter

  @enforce_keys [:max_tries, :max_backoff, :min_backoff, :policy]
  defstruct [:max_tries, :max_backoff, :min_backoff, :policy]
  @max_tries 3
  @max_backoff 0

  @typedoc """
  Retry struct.
  """
  @type t :: %Retry{
          policy: (Exception.t() -> boolean),
          max_tries: pos_integer,
          max_backoff: non_neg_integer,
          min_backoff: non_neg_integer
        }

  @typedoc """
  Retry options.
  """
  @type option ::
          {:max_tries, pos_integer}
          | {:max_backoff, non_neg_integer}
          | {:min_backoff, non_neg_integer}

  @doc """
  Create a retry configuration from a `policy` fun and options.

  The `policy` fun determines whether a given exception is retryable (returns `true`)
  or not (return `false`).

  ## Options
      * `:max_tries` - maximum number of tries (including first try) (default: `3`)
      * `:max_backoff` - maximum backoff time between tries (default: `0`)
      * `:min_backoff` - minimum backoff time between tries (default based on
      `:max_tries` and `:max_backoff`)

  The delay between tries uses exponenial random backoff, where the mean delay doubles
  for each try and is uniformly chosen in the interval
  `[0.5 * mean delay, 1.5 * mean delay]`. It is rare that `:min_backoff` is set as
  it can be calculated working backwards from `:max_backoff` and `:max_tries`.

  Also if `Stack.Deadline` is active the deadline will be honored so that a retry or
  backoff won't start if the deadline can't be met.
  """
  @spec new((Exception.t() -> boolean)) :: t
  def new(policy, opts \\ [])

  @spec new((Exception.t() -> boolean), [option]) :: t
  def new(policy, opts) when is_function(policy, 1) do
    max_tries = get_integer(opts, :max_tries, 1, :infinity, @max_tries)
    max_backoff = get_integer(opts, :max_backoff, 0, :infinity, @max_backoff)
    # max_tries - 1 possible backoffs
    # final backoff is bounded by max so for exponential backoff the nth attempt upper bound is:
    # max backoff /  2 ^ (possible backoffs - n) = max backoff / 2 ^ (max_tries - 1 - n)
    # Therefore upper bound of first backoff is max backoff / 2 ^ (max tries - 2)
    # If upper is 1.5X in range [0.5X, 1.5X] then lower bound is upper / 3
    # Therefore default min backoff is:
    default_min_backoff =
      div(max_backoff >>> max(max_tries - 2, 0), 3)
      |> max(1)
      |> min(max_backoff)

    min_backoff =
      get_integer(opts, :min_backoff, min(max_backoff, 1), max_backoff, default_min_backoff)

    %Retry{
      policy: policy,
      max_tries: max_tries,
      max_backoff: max_backoff,
      min_backoff: min_backoff
    }
  end

  @doc """
  Bind a retry to the scope of an anonymous function and run the function.

  The first argument is a retry struct. If the fun raises an exception it may retry and/or
  backoff depending on the policy set in the retry struct.

  ## Examples

      Stack.Retry.bind(Stack.Retry.new(fn %err{} -> err == RuntimeError end), fn ->
        Database.fetch!(key)
      end)
  """
  @spec bind(t, (() -> result)) :: result when result: var
  def bind(%Retry{} = retry, fun) do
    bind(retry, 1, fun)
  end

  @doc """
  Get the current number of tries from the retries in the current scope.

  If a retry is not bound in the current scope returns `1`.
  """
  @spec tries() :: pos_integer
  def tries() do
    Context.get(Retry, 1)
  end

  @doc """
  Create a filter thay binds retry handling inside the scope of a filter.

  ## Examples

      Stack.Retry.filter(fn %err{} -> err == RuntimeError end, [max_tries: 2])
      |> Filter.into(fn -> Database.fetch!(key) end)

  """
  @spec filter(t | (Exception -> boolean)) :: Filter.t(req, rep, req, rep) when req: var, rep: var
  def filter(%Retry{} = retry) do
    Filter.new(Retry, retry)
  end

  def filter(filter) when is_function(filter, 1) do
    filter(filter, [])
  end

  @spec filter((Exception.t() -> boolean), [option]) :: Filter.t(req, rep, req, rep)
        when req: var, rep: var
  def filter(filter, options) do
    new(filter, options)
    |> filter()
  end

  @doc false
  @impl Filter
  def init(retry), do: retry

  @doc false
  @impl Filter
  def call(req, service, retry) do
    bind(retry, fn -> service.(req) end)
  end

  ## Helper

  defp get_integer(opts, key, min, max, default) do
    case Keyword.get(opts, key, default) do
      int when is_integer(int) and min <= int and int <= max ->
        int

      other when max == :infinity ->
        raise ArgumentError, "expected #{min} <= #{inspect(key)}, got: #{inspect(other)}"

      other ->
        raise ArgumentError,
              "expected #{min} <= #{inspect(key)} <= #{inspect(max)}, got: #{inspect(other)}"
    end
  end

  defp bind(%Retry{max_tries: tries}, tries, fun) do
    Context.bind(Retry, tries, fun)
  end

  defp bind(%Retry{policy: policy} = retry, attempt, fun) do
    Context.bind(Retry, attempt, fun)
  catch
    :error, error ->
      stack = System.stacktrace()
      exception = Exception.normalize(:error, error, stack)
      timeout = Deadline.timeout()

      if timeout > 0 and policy.(exception) and backoff(retry, attempt, timeout) do
        bind(retry, attempt + 1, fun)
      else
        reraise exception, stack
      end
  end

  defp backoff(%Retry{max_backoff: 0}, _, _), do: true

  defp backoff(%Retry{max_backoff: 1}, _, _) do
    :timer.sleep(1)
    true
  end

  defp backoff(%Retry{max_backoff: max_backoff, min_backoff: min_backoff}, attempt, timeout) do
    # Backoff randomly in interval [0.5X, 1.5X] to minimise collisions
    # [upper/3, upper] = upper/3 + [0, 2 * upper/3]
    max_range = div(max_backoff <<< 1, 3)
    # Exponential backoff for each try, where min_backoff is upper/3 for first try
    range = min(min_backoff <<< attempt, max_range)
    lower = range >>> 1
    backoff = max(lower + :rand.uniform(range), min_backoff)

    # Only backoff if it ends inside the deadline
    if backoff < timeout do
      :timer.sleep(backoff)
      true
    else
      false
    end
  end
end
