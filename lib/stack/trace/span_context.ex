defmodule Stack.Trace.SpanContext do
  @moduledoc """
  Trace span context.
  """
  alias Stack.Trace
  alias Trace.{SpanContext, SpanData, Sampler, Probability}
  use Bitwise

  @uint128_max (1 <<< 128) - 1
  @uint64_max (1 <<< 64) - 1

  @enforce_keys [:trace_id, :parent_id, :span_id]
  defstruct [:trace_id, :parent_id, :span_id, trace_options: [], data: nil]

  @type t :: %SpanContext{
          trace_id: Trace.trace_id(),
          parent_id: Trace.span_id(),
          span_id: Trace.span_id(),
          trace_options: [Trace.trace_option()],
          data: nil | SpanData.t()
        }

  @spec new(name) :: t() when name: String.t()
  @spec new(name, [Trace.span_option()]) :: t() when name: String.t()
  def new(name, span_opts \\ [])

  def new(name, span_opts) do
    trace_id = uint128()
    span_id = trace_id &&& 0xFFFF_FFFF_FFFF_FFFF
    new(trace_id, span_id, span_id, root_opts(span_opts), name, span_opts)
  end

  @spec new(t(), name, [Trace.span_option()]) :: t() when name: String.t()
  def new(
        %SpanContext{trace_id: trace_id, span_id: span_id, trace_options: trace_opts},
        name,
        span_opts
      ) do
    new(trace_id, span_id, uint64(), trace_opts, name, span_opts)
  end

  @spec new(Trace.trace_id(), Trace.span_id(), Trace.span_id(), [Trace.trace_option()], name) :: t
        when name: String.t()
  @spec new(Trace.trace_id(), Trace.span_id(), Trace.span_id(), [Trace.trace_option()], name, [
          Trace.span_option()
        ]) :: t
        when name: String.t()
  def new(trace_id, parent_id, span_id, trace_opts, name, span_opts \\ []) do
    %SpanContext{
      trace_id: trace_id,
      parent_id: parent_id,
      span_id: span_id,
      trace_options: trace_opts,
      data: new_data(name, trace_id, parent_id, span_id, trace_opts, span_opts)
    }
  end

  @spec recording?(t()) :: boolean
  def recording?(%SpanContext{data: data}), do: data != nil

  @spec put_attribute(t(), key, value) :: boolean when key: term, value: term
  def put_attribute(%SpanContext{data: nil}, _, _), do: false

  def put_attribute(%SpanContext{data: data}, key, value) do
    SpanData.put(data, key, value)
  end

  @spec put_attributes(t(), [{key, value}]) :: boolean when key: term, value: term
  def put_attributes(%SpanContext{data: nil}, _), do: false

  def put_attributes(%SpanContext{data: data}, attributes) do
    SpanData.put(data, attributes)
  end

  @spec delete(t()) :: boolean
  def delete(%SpanContext{data: nil}), do: false

  def delete(%SpanContext{data: data}) do
    SpanData.delete(data)
  end

  ## Helpers

  defp root_opts(span_opts) do
    sampler = Keyword.get_lazy(span_opts, :sampler, &Probability.new/0)

    case Sampler.sample?(sampler) do
      true ->
        [:sampled]

      false ->
        []
    end
  end

  defp new_data(name, trace_id, parent_id, span_id, trace_opts, span_opts) do
    if Enum.member?(trace_opts, :sampled) do
      span_kind = Keyword.get(span_opts, :span_kind)
      data = SpanData.new(span_opts)

      SpanData.put(
        data,
        name: name,
        trace_id: trace_id,
        parent_id: parent_id,
        span_id: span_id,
        start_time: System.monotonic_time(),
        span_kind: span_kind
      )

      data
    end
  end

  defp uint128(), do: :rand.uniform(@uint128_max + 1) - 1

  defp uint64(), do: :rand.uniform(@uint64_max + 1) - 1
end
