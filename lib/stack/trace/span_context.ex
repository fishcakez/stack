defmodule Stack.Trace.SpanContext do
  alias Stack.{Trace, Context, Filter}
  alias Stack.Trace.{SpanContext, Sampler, Probability}
  use Bitwise
  @behaviour Filter

  @uint128_max (1 <<< 128) - 1
  @uint64_max (1 <<< 64) - 1
  @time_unit 1_000_000_000

  @enforce_keys [:trace_id, :parent_id, :span_id]
  defstruct [:trace_id, :parent_id, :span_id, trace_options: [], data: nil]

  @type t :: %SpanContext{
          trace_id: Trace.trace_id(),
          parent_id: Trace.span_id(),
          span_id: Trace.span_id(),
          trace_options: [Trace.trace_option()],
          data: nil | :ets.tid()
        }

  @spec new(name) :: t() when name: String.t()
  @spec new(name, [Trace.span_option()]) :: t() when name: String.t()
  def new(name, span_opts \\ [])

  def new(name, span_opts) do
    case Context.fetch(SpanContext) do
      {:ok, parent} ->
        new(parent, name, span_opts)

      :error ->
        trace_id = uint128()
        span_id = trace_id &&& 0xFFFF_FFFF_FFFF_FFFF
        new(trace_id, span_id, span_id, root_opts(span_opts), name, span_opts)
    end
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
      data: new_data(name, trace_opts, span_opts)
    }
  end

  @spec bind(t(), (() -> result)) :: result when result: var
  def bind(%SpanContext{} = span, fun) do
    Context.bind(SpanContext, span, fun)
  end

  @spec recording?() :: boolean
  def recording?() do
    case Context.fetch(SpanContext) do
      {:ok, %SpanContext{data: data}} ->
        data != nil

      :error ->
        false
    end
  end

  @spec recording?(t()) :: boolean
  def recording?(%SpanContext{data: data}), do: data != nil

  @spec delete(t()) :: :ok
  def delete(%SpanContext{data: nil}), do: :ok

  def delete(%SpanContext{data: data}) do
    :ets.delete(data)
    :ok
  end

  @doc false
  @impl Filter
  def init(arg), do: arg

  @doc false
  @impl Filter
  def call(req, service, {name, opts}) do
    case new(name, opts) do
      %SpanContext{data: nil} = span ->
        bind(span, fn -> service.(req) end)

      %SpanContext{data: data} = span ->
        bind(span, fn ->
          try do
            service.(req)
          after
            :ets.delete(data)
          end
        end)
    end
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

  defp new_data(name, trace_opts, span_opts) do
    if Keyword.get(trace_opts, :sampled, false) do
      data = :ets.new(SpanContext, [])
      span_kind = Keyword.get(span_opts, :span_kind, :unspecified)

      :ets.insert(data, [
        {:start_time, System.monotonic_time(@time_unit)},
        {:name, name},
        {:span_kind, span_kind}
      ])

      data
    end
  end

  defp uint128(), do: :rand.uniform(@uint128_max + 1) - 1

  defp uint64(), do: :rand.uniform(@uint64_max + 1) - 1
end
