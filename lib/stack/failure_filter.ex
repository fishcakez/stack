defmodule Stack.FailureFilter do
  @moduledoc """
  Retry handling with Stack.Filter.

  This module provides retry, backoff and rejection handling via a Stack.Filter that takes into account
  deadlines and the criticality of a request.
  """
  alias Stack.{
    Failure,
    FailureFilter,
    RetryBudget,
    RejectBudget,
    Context,
    Criticality,
    Deadline,
    Filter,
    Drop
  }

  use Bitwise
  @behaviour Filter

  defmodule Config do
    @moduledoc false

    @max_tries 3
    @backoff_threshold 1
    @retry_threshold :critical
    @force_threshold :critical

    @enforce_keys [
      :max_tries,
      :backoff,
      :backoff_threshold,
      :policy,
      :retry_budget,
      :retry_threshold,
      :reject_budget,
      :force_threshold
    ]
    defstruct [
      :max_tries,
      :backoff,
      :backoff_threshold,
      :policy,
      :retry_budget,
      :retry_threshold,
      :reject_budget,
      :force_threshold
    ]

    def new(policy, opts) do
      %Config{
        policy: policy,
        max_tries: pos_integer(opts, :max_tries, @max_tries),
        backoff: Keyword.get(opts, :backoff),
        backoff_threshold: pos_integer(opts, :backoff_threshold, @backoff_threshold),
        retry_budget: budget(opts, :retry_budget),
        retry_threshold: criticality(opts, :retry_threshold, @retry_threshold),
        reject_budget: budget(opts, :reject_budget),
        force_threshold: criticality(opts, :force_threshold, @force_threshold)
      }
    end

    defp pos_integer(opts, key, default) do
      case Keyword.get(opts, key, default) do
        int when is_integer(int) and 0 < int and int < :infinity ->
          int

        other ->
          raise ArgumentError, "expected integer > 0 for #{inspect(key)}, got: #{inspect(other)}"
      end
    end

    defp budget(opts, key) do
      case Keyword.get(opts, key) do
        name when is_atom(name) ->
          name

        other ->
          raise ArgumentError, "expected atom name for {#inspect(key)}, got: #{inspect(other)}"
      end
    end

    defp criticality(opts, key, default) do
      case Keyword.get(opts, key, default) do
        level when level in [:sheddable, :sheddable_plus, :critical, :critical_plus] ->
          level

        other ->
          raise ArgumentError,
                "expected criticality level for #{inspect(key)}, got: #{inspect(other)}"
      end
    end
  end

  @typedoc """
  Retry options.
  """
  @type option ::
          {:max_tries, pos_integer}
          | {:backoff, :backoff.backoff()}
          | {:backoff_threshold, pos_integer}
          | {:retry_budget, atom}
          | {:retry_threshold, Criticality.t()}
          | {:reject_budget, atom}
          | {:force_threshold, Criticality.t()}

  @doc """
  Create a failure filter that handles failure, such as retries and rejection.

  The `policy` fun determines whether the reply is `:cont`, `:reject`, `:retry` or `:error`.

  ## Examples

   Stack.FailureFilter.new(fn %WriteError{} -> :retry ; %{__exception__: true} -> :error ; _ -> :cont end, [max_tries: 2])
   |> Filter.into(fn -> Database.fetch!(key) end)

  ## Options
      * `:max_tries` - maximum number of tries (including first try) (default: `3`)
      * `:backoff` - backoff data structure, see `:backoff`, no backoff if `nil` (default: `nil`)
      * `:backoff_threshold` - minimum timeout remaining in deadline to allow backoff (default: `1`)
      * `:retry_budget` - retry budget to limit retries, see `Stack.RetryBudget` (default: `nil`)
      * `:retry_threshold` - minimium criticality level to retry a failure (default: `:critical`)
      * `:reject_budget` - reject budget to pre-emptively reject, see `Stack.RejectBudget` (default: `nil`)
      * `:force_threshold` - minimum criticality level to force a budget not to drop (default: `:critical`)

  If `Stack.Deadline` is active, the deadline will be honored for retries. If there is less than `backoff_threshold`
  remaining in the deadline after a retry backoff the filter will not retry. By default a retry will occur if the
  deadline has not expired after the backoff finishes.

  If `Stack.Criticality` is bound in the current scope, requests will only be retried at the `retry_threshold` or
  above and requests will not be dropped by the reject budget at the `force_threshold` or above. The default for
  both thresholds is `:critical`.
  """
  @spec new((Exception -> :cont | Failure.type()), [option]) :: Filter.t(req, rep, req, rep)
        when req: var, rep: var
  def new(policy, opts \\ []) when is_function(policy, 1) do
    config = Config.new(policy, opts)

    Filter.new(FailureFilter, config)
  end

  @doc """
  Get the current number of tries in the current scope.

  If tries is not bound in the current scope returns `1`.
  """
  @spec tries() :: pos_integer
  def tries() do
    Context.get(FailureFilter.Tries, 1)
  end

  @doc false
  @impl Filter
  def init(config), do: config

  @doc false
  @impl Filter
  def call(req, service, config) do
    bind(config, fn -> service.(req) end)
  end

  ## Helper

  defp bind(%Config{backoff: backoff} = config, fun) do
    bind(config, 1, backoff, fun)
  end

  defp bind(%Config{policy: policy} = config, tries, backoff, fun) do
    level = Criticality.level()
    {reject_tid, retry_tid} = ask(config, level)
    result = Context.bind(FailureFilter.Tries, tries, fun)
    type = policy.(result)

    case type do
      :reject ->
        ack(reject_tid, &RejectBudget.nack(&1, tries))

      _ ->
        ack(reject_tid, &RejectBudget.ack/1)
    end

    case retry?(config, tries, backoff, level, retry_tid, type) do
      true ->
        bind(config, tries + 1, sleep(backoff), fun)

      false when type == :cont ->
        result

      false ->
        raise Failure, type: type, value: result, criticality: level
    end
  end

  defp retry?(config, tries, backoff, level, retry_tid, type) do
    if type in [:reject, :retry] and retry?(config, tries, backoff, level) do
      ack(retry_tid, &RetryBudget.nack(&1, tries))
    else
      ack(retry_tid, &RetryBudget.ack/1)
      false
    end
  end

  defp retry?(config, tries, backoff, level) do
    %Config{max_tries: max_tries, retry_threshold: retry_threshold} = config

    tries < max_tries and backoff?(config, backoff) and
      Criticality.compare_levels(level, retry_threshold) in [:eq, :gt]
  end

  defp backoff?(%Config{backoff_threshold: threshold}, backoff) do
    timeout = Deadline.timeout()

    if is_nil(backoff) do
      timeout >= threshold
    else
      delay = :backoff.get(backoff)
      timeout >= threshold + delay
    end
  end

  defp sleep(nil), do: nil

  defp sleep(backoff) do
    delay = :backoff.get(backoff)
    :timer.sleep(delay)
    {_, backoff} = :backoff.fail(backoff)
    backoff
  end

  defp ask(config, level) do
    %Config{
      reject_budget: reject_budget,
      force_threshold: force_threshold,
      retry_budget: retry_budget
    } = config

    case Criticality.compare_levels(level, force_threshold) do
      :lt ->
        # Ask reject budget first to prevent retry budget request count increment if reject budget drops
        reject_tid = ask(reject_budget, &RejectBudget.ask/1, level)
        {reject_tid, ask(retry_budget, &RetryBudget.ask/1, level)}

      _ ->
        reject_tid = ask(reject_budget, &RejectBudget.force/1, level)
        {reject_tid, ask(retry_budget, &RetryBudget.force/1, level)}
    end
  end

  defp ask(nil, _, _), do: nil

  defp ask(name, ask, level) do
    case ask.(name) do
      {:go, tid} ->
        tid

      :drop ->
        raise Failure, type: :reject, value: Drop.exception(process: name), criticality: level
    end
  end

  defp ack(nil, _), do: true

  defp ack(tid, ack), do: ack.(tid)
end
