defmodule Stack.Trace do
  alias Stack.{Context, Trace}
  use Bitwise

  @uint128_max (1 <<< 128) - 1
  @uint64_max (1 <<< 64) - 1

  @enforce_keys [:trace_id, :parent_id, :span_id, :flags]
  defstruct [:trace_id, :parent_id, :span_id, :flags]

  @type trace_id :: 0..unquote(@uint128_max)
  @type span_id :: 0..unquote(@uint64_max)
  @type flag :: :debug | :sampling_known | :sampled | :root
  @type t :: %Trace{trace_id: trace_id, parent_id: span_id, span_id: span_id, flags: [flag]}

  @spec start(t) :: t
  @spec start([flag]) :: t
  def start(%Trace{trace_id: trace_id, span_id: parent_id, flags: flags}) do
    start(trace_id, parent_id, flags)
  end

  def start(flags) do
    trace_id = uint128()
    span_id = trace_id &&& unquote(@uint64_max)
    join(trace_id, span_id, span_id, [:root | flags])
  end

  @spec start(trace_id, span_id, [flag]) :: t
  def start(trace_id, parent_id, flags) do
    join(trace_id, parent_id, uint64(), Enum.filter(flags, &(&1 != :root)))
  end

  @spec join(trace_id, span_id, span_id, [flag]) :: t
  def join(trace_id, parent_id, span_id, flags) do
    %Trace{trace_id: trace_id, parent_id: parent_id, span_id: span_id, flags: flags}
  end

  @spec bind(t, (() -> result)) :: result when result: var
  @spec bind(t, (... -> result), list) :: result when result: var
  def bind(trace, fun, args \\ [])

  def bind(%Trace{} = trace, fun, args) do
    if Context.has_key?(Trace) do
      raise "#{inspect(Trace)} already bound in #{inspect(Context)}"
    else
      Context.bind(Trace, trace, fun, args)
    end
  end

  @spec bind(trace_id, span_id, span_id, [flag], (() -> result)) :: result when result: var
  @spec bind(trace_id, span_id, span_id, [flag], (... -> result), list) :: result when result: var
  def bind(trace_id, parent_id, span_id, flags, fun, args \\ []) do
    trace = join(trace_id, parent_id, span_id, flags)
    bind(trace, fun, args)
  end

  @spec span((() -> result)) :: result when result: var
  def span(fun) when is_function(fun) do
    span(fun, [])
  end

  @spec span((... -> result), list) :: result when result: var
  def span(fun, args) when is_function(fun) do
    trace = Context.fetch!(Trace)
    Context.bind(Trace, start(trace), fun, args)
  end

  @spec span(t | [flag], (() -> result)) :: result when result: var
  def span(trace_or_flags, fun) do
    span(trace_or_flags, fun, [])
  end

  @spec span(t | [flag], (... -> result), list) :: result when result: var
  def span(trace_or_flags, fun, args) do
    trace_or_flags
    |> start()
    |> bind(fun, args)
  end

  @spec into() :: (req, (req -> rep) -> rep) when req: var, rep: var
  def into(), do: &span(&2, [&1])

  @spec into([flag]) :: (req, (req -> rep) -> rep) when req: var, rep: var
  def into(flags), do: &span(flags, &2, [&1])

  ## Helpers

  defp uint128(), do: :rand.uniform(@uint128_max + 1) - 1

  defp uint64(), do: :rand.uniform(@uint64_max + 1) - 1
end
