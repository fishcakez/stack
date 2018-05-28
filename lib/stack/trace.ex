defmodule Stack.Trace do
  @moduledoc """
  Trace context via Stack.Context.

  This modules provides spans over (multiple) Stack.Contexts.
  """
  alias Stack.{Context, Trace}
  alias Trace.{Sampler, SpanContext, SpanData}
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
  @type span_kind :: :client | :server | nil

  @typedoc """
  Span options.
  """
  @type span_option ::
          {:sampler, Sampler.t()}
          | {:span_kind, span_kind()}
          | {:span_data, :ets.access()}
          | {:span_deadline, Deadline.t()}

  @spec new(name) :: SpanContext.t() when name: String.t()
  @spec new(name, [Trace.span_option()]) :: SpanContext.t() when name: String.t()
  def new(name, span_opts \\ [])

  def new(name, span_opts) do
    case Context.fetch(Trace) do
      {:ok, parent} ->
        SpanContext.new(parent, name, deadline(span_opts))

      :error ->
        SpanContext.new(name, deadline(span_opts))
    end
  end

  @spec span(name | SpanContext.t(), (() -> result)) :: result when name: String.t(), result: var
  @spec span(name | SpanContext.t(), (() -> result), [span_option]) :: result
        when name: String.t(), result: var
  def span(name, fun, span_opts \\ [])

  def span(%SpanContext{} = span_ctx, fun, _span_opts) do
    Context.bind_defer(Trace, span_ctx, fun, fn -> SpanContext.delete(span_ctx) end)
  end

  def span(name, fun, span_opts) when is_binary(name) do
    name
    |> new(span_opts)
    |> span(fun, span_opts)
  end

  @spec bind(SpanContext.t(), (() -> result)) :: result when result: var
  def bind(span_ctx, fun) do
    Context.bind(Trace, span_ctx, fun)
  end

  @spec recording?() :: boolean
  def recording?() do
    case Context.fetch(Trace) do
      {:ok, %SpanContext{data: data}} ->
        data != nil

      :error ->
        false
    end
  end

  @spec put_attribute(key, value) :: boolean when key: term, value: term
  def put_attribute(key, value) do
    case Context.get(Trace) do
      %SpanContext{data: data} when data != nil ->
        SpanData.put(data, key, value)

      _ ->
        false
    end
  end

  @spec put_attributes([{key, value}]) :: boolean when key: term, value: term
  def put_attributes(attributes) do
    case Context.get(Trace) do
      %SpanContext{data: data} when data != nil ->
        SpanData.put(data, attributes)

      _ ->
        false
    end
  end

  defp deadline(span_opts) do
    with false <- Keyword.has_key?(span_opts, :span_deadline),
         {:ok, deadline} <- Context.fetch(Deadline) do
      [span_deadline: deadline] ++ span_opts
    else
      _ ->
        span_opts
    end
  end
end
