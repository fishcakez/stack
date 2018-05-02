defmodule Stack.Trace do
  @moduledoc """
  Trace context with Stack.Filter via Stack.Context.

  This modules provides spans over (multiple) Stack.Contexts.
  """
  alias Stack.Filter
  alias Stack.Trace.{Sampler, SpanContext}
  use Bitwise

  @uint128_max (1 <<< 128) - 1
  @uint64_max (1 <<< 64) - 1

  @typedoc """
  Trace id of all spans in a trace.
  """
  @type trace_id :: 0..unquote(@uint128_max)

  @typedoc """
  Span id of a single span in a trace.
  """
  @type span_id :: 0..unquote(@uint64_max)

  @typedoc """
  Trace options.
  """
  @type trace_option :: :sampled

  @typedoc """
  Span kind.
  """
  @type span_kind :: :client | :server | :unspecified

  @typedoc """
  Span options.
  """
  @type span_option :: {:sampler, Sampler.t()} | {:span_kind, span_kind()}

  @doc """
  Create a filter that starts a span inside the scope of filter.

  ## Examples

      Stack.Trace.filter("rpc")
      |> Filter.into(fn -> RPC.call() end)
  """
  @spec filter(name) :: Filter.t(req, rep, req, rep)
        when name: String.t(), req: var, rep: var
  @spec filter(name, [span_option]) :: Filter.t(req, rep, req, rep)
        when name: String.t(), req: var, rep: var
  def filter(name, opts \\ []) do
    Filter.new(SpanContext, {name, opts})
  end
end
