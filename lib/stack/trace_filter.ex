defmodule Stack.TraceFilter do
  @moduledoc """
  Trace context with Stack.Filter via Stack.Context.

  This modules provides spans over (multiple) Stack.Contexts.
  """
  alias Stack.{Filter, TraceFilter, Trace}
  @behaviour Filter

  @doc """
  Create a filter that starts a span inside the scope of filter.

  ## Examples

      Stack.TraceFilter.new("rpc")
      |> Filter.into(fn -> RPC.call() end)
  """
  @spec new(name) :: Filter.t(req, rep, req, rep)
        when name: String.t(), req: var, rep: var
  @spec new(name, [Trace.span_option()]) :: Filter.t(req, rep, req, rep)
        when name: String.t(), req: var, rep: var
  def new(name, span_opts \\ []) do
    Filter.new(TraceFilter, {name, span_opts})
  end

  @doc false
  @impl Filter
  def init(arg), do: arg

  @doc false
  @impl Filter
  def call(req, service, {name, span_opts}) do
    Trace.span(name, fn -> service.(req) end, span_opts)
  end
end
