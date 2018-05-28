defmodule Stack.TraceTest do
  alias Stack.{Context, Trace, TraceFilter, Service, Filter}
  alias Stack.Trace.{SpanContext, Probability}
  use ExUnit.Case, async: true
  use Bitwise

  test "span with flags starts a root trace" do
    Trace.span("test", fn ->
      span = Context.fetch!(Trace)
      assert %SpanContext{parent_id: span_id, span_id: span_id} = span
      assert (span.trace_id &&& 0xFFFF_FFFF_FFFF_FFFF) == span_id
    end)
  end

  test "nested span inherits trace_id and the parent_id is parent's span_id" do
    Trace.span(SpanContext.new(1, 2, 3, [], "test"), fn ->
      Trace.span("inner", fn ->
        span = Context.fetch!(Trace)
        assert %SpanContext{trace_id: 1, parent_id: 3, span_id: span_id, trace_options: []} = span
        assert span_id !== 3
      end)
    end)
  end

  test "span started with transform filter" do
    service1 =
      Service.new()
      |> Service.map(fn n -> {n + 1, Context.fetch!(Trace)} end)

    service2 =
      Filter.new()
      |> Filter.transform(TraceFilter.new("test", sampler: Probability.new(1.0)))
      |> Filter.into(service1)

    assert {2, %SpanContext{trace_options: [:sampled]}} = Service.init(service2).(1)
  end

  test "nested span inherits trace id with transform filter" do
    service1 =
      Service.new()
      |> Service.map(fn n -> {n + 1, Context.fetch!(Trace)} end)

    service2 =
      Filter.new()
      |> Filter.transform(TraceFilter.new("test"))
      |> Filter.into(service1)

    Trace.span(SpanContext.new(1, 2, 3, [], "test"), fn ->
      assert {2, span} = Service.init(service2).(1)
      assert %SpanContext{trace_id: 1, parent_id: 3, span_id: span_id, trace_options: []} = span
      assert span_id !== 3
    end)
  end
end
