defmodule Stack.ConcurrencyFilter do
  @moduledoc """
  Concurrency limiting with Stack.Filter.

  This module provides concurrency limiting via Stack.Filter.
  """
  alias Stack.{Filter, Criticality, Drop, Failure, ConcurrencyFilter}
  @behaviour Filter

  defmodule Config do
    @moduledoc false

    @block_threshold :critical

    @enforce_keys [:regulator, :block_threshold]
    defstruct [:regulator, :block_threshold]
    @type t :: %Config{regulator: :sregulator.regulator(), block_threshold: Criticality.t()}

    def new(regulator, opts) do
      %Config{
        regulator: regulator,
        block_threshold: criticality(opts, :block_threshold, @block_threshold)
      }
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
  Concurrency options.
  """
  @type option :: {:block_threshold, Criticality.t()}

  @doc """
  Create a filter that limits concurrency via an `:sregulator`.

  ## Examples

      Stack.ConcurrencyFilter.new(APIRegulator, [block_threshold: :sheddable_plus])
      |> Filter.into(fn -> API.get!(resource) end)

  ## Options
      * `:block_threshold` - minimium criticality level to block waiting for a concurrency lock (default: `:critical`)

  If `Stack.Criticality` is bound in the current scope, requests will block to wait for a concurrency lock at the
  `block_threshold` or above. Otherwise they will not block and can only continue if lock immediately available.
  """
  @spec new(:sregulator.regulator(), [option]) :: Filter.t(req, rep, req, rep)
        when req: var, rep: var
  def new(regulator, opts \\ []) do
    config = Config.new(regulator, opts)
    Filter.new(ConcurrencyFilter, config)
  end

  @doc false
  @impl Filter
  def init(config), do: config

  @doc false
  @impl Filter
  def call(req, service, config) do
    {pid, ref} = ask(config)

    try do
      service.(req)
    after
      :sregulator.dirty_done(pid, ref)
    end
  end

  defp ask(%Config{regulator: regulator, block_threshold: block_threshold}) do
    level = Criticality.level()

    case Criticality.compare_levels(level, block_threshold) do
      :lt ->
        ask(regulator, &:sregulator.nb_ask/1, level)

      _ ->
        ask(regulator, &:sregulator.ask/1, level)
    end
  end

  defp ask(regulator, ask, level) do
    case ask.(regulator) do
      {:go, ref, pid, _, _} ->
        {pid, ref}

      {:drop, _} ->
        raise Failure,
          type: :reject,
          value: Drop.exception(process: regulator),
          criticality: level
    end
  end
end
