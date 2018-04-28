defmodule Stack.Trace do
  @moduledoc """
  Trace context with Stack.Filter via Stack.Context.

  This modules provides spans over (multiple) Stack.Contexts.
  """
  alias Stack.{Context, Filter, Trace}
  use Bitwise
  @behaviour Filter

  @uint128_max (1 <<< 128) - 1
  @uint64_max (1 <<< 64) - 1

  @enforce_keys [:trace_id, :parent_id, :span_id, :flags]
  defstruct [:trace_id, :parent_id, :span_id, :flags]

  @typedoc """
  Trace id of all spans in a trace.
  """
  @type trace_id :: 0..unquote(@uint128_max)

  @typedoc """
  Span id of a single span in a trace.
  """
  @type span_id :: 0..unquote(@uint64_max)

  @typedoc """
  Trace flags.
  """
  @type flag :: :debug | :sampling_known | :sampled | :root

  @typedoc """
  Trace struct.
  """
  @type t :: %Trace{trace_id: trace_id, parent_id: span_id, span_id: span_id, flags: [flag]}

  @doc """
  Start a new span in a trace.

  If passed a trace struct starts a new struct. Otherwise when passed flags, starts a span under new trace.

  This function should be called on the client.
  """
  @spec start(t) :: t
  def start(%Trace{trace_id: trace_id, span_id: parent_id, flags: flags}) do
    start(trace_id, parent_id, flags)
  end

  @spec start([flag]) :: t
  def start(flags) do
    trace_id = uint128()
    span_id = trace_id &&& unquote(@uint64_max)
    join(trace_id, span_id, span_id, [:root | flags])
  end

  @doc """
  Start a new span in a trace.

  This function should be called on the client.
  """
  @spec start(trace_id, span_id, [flag]) :: t
  def start(trace_id, parent_id, flags) do
    join(trace_id, parent_id, uint64(), Enum.filter(flags, &(&1 != :root)))
  end

  @doc """
  Join an existing span in a trace.

  This function should be called on the server.
  """
  @spec join(trace_id, span_id, span_id, [flag]) :: t
  def join(trace_id, parent_id, span_id, flags) do
    %Trace{trace_id: trace_id, parent_id: parent_id, span_id: span_id, flags: flags}
  end

  @doc """
  Run a fun in a new span in the trace in the current scope.

  If a trace isn't bound in the current scope raises `KeyError`.
  """
  @spec span((() -> result)) :: result when result: var
  def span(fun) when is_function(fun) do
    trace = Context.fetch!(Trace)
    Context.bind(Trace, start(trace), fun)
  end

  @doc """
  Run a fun in a new span in a new trace in the current scope.

  If a trace is bound in the current scopes raises `RuntimeError`.
  """
  @spec span(t | [flag], (() -> result)) :: result when result: var
  def span(trace_or_flags, fun) do
    trace_or_flags
    |> start()
    |> bind(fun)
  end

  @doc """
  Create a filter that starts a span inside the scope of filter.

  ## Examples

      Stack.Trace.filter()
      |> Filter.into(fn -> RPC.call() end)
  """
  @spec filter() :: Filter.t(req, rep, req, rep) when req: var, rep: var
  def filter() do
    Filter.new(Trace, :child)
  end

  @doc """
  Create a filter that starts a span in a new trace inside the scope of filter.

  ## Examples

      Stack.Trace.filter()
      |> Filter.into(fn -> RPC.call() end)
  """
  @spec filter([flag]) :: Filter.t(req, rep, req, rep) when req: var, rep: var
  def filter(flags) when is_list(flags) do
    Filter.new(Trace, {:parent, flags})
  end

  @doc false
  @impl Filter
  def init(arg), do: arg

  @doc false
  @impl Filter
  def call(req, service, :child) do
    span(fn -> service.(req) end)
  end

  def call(req, service, {:parent, flags}) do
    span(flags, fn -> service.(req) end)
  end

  ## Helpers

  defp uint128(), do: :rand.uniform(@uint128_max + 1) - 1

  defp uint64(), do: :rand.uniform(@uint64_max + 1) - 1

  def bind(%Trace{} = trace, fun) do
    if Context.has_key?(Trace) do
      raise "#{inspect(Trace)} already bound in #{inspect(Context)}"
    else
      Context.bind(Trace, trace, fun)
    end
  end

  def bind(trace_id, parent_id, span_id, flags, fun) do
    trace = join(trace_id, parent_id, span_id, flags)
    bind(trace, fun)
  end
end
