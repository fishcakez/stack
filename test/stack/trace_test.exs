defmodule Stack.TraceTest do
  alias Stack.{Context, Trace, Service, Filter}
  use ExUnit.Case, async: true
  use Bitwise

  test "span with flags starts a root trace" do
    Trace.span([:debug], fn ->
      trace = Context.fetch!(Trace)
      assert %Trace{parent_id: span_id, span_id: span_id} = trace
      assert (trace.trace_id &&& 0xFFFF_FFFF_FFFF_FFFF) == span_id
      assert Enum.member?(trace.flags, :root)
      assert Enum.member?(trace.flags, :debug)
    end)
  end

  test "bind raises if trace already bound" do
    Trace.span([], fn ->
      assert_raise RuntimeError, "Stack.Trace already bound in Stack.Context", fn ->
        Trace.bind(Trace.start([]), fn -> flunk("ran") end)
      end
    end)
  end

  test "nested span inherits trace_id and the parent_id is parent's span_id" do
    Trace.bind(1, 2, 3, [], fn ->
      Trace.span(fn ->
        trace = Context.fetch!(Trace)
        assert %Trace{trace_id: 1, parent_id: 3, span_id: span_id, flags: []} = trace
        assert span_id !== 3
      end)
    end)
  end

  test "span started with into filter" do
    service1 =
      Service.new()
      |> Service.map(fn n -> {n + 1, Context.fetch!(Trace)} end)

    service2 =
      Filter.new()
      |> Filter.into(Trace.into([:debug]))
      |> Filter.into(service1)

    assert {2, %Trace{flags: flags}} = Service.call(service2, 1)
    assert Enum.member?(flags, :root)
    assert Enum.member?(flags, :debug)
  end

  test "nested span inherits trace id with into filter" do
    service1 =
      Service.new()
      |> Service.map(fn n -> {n + 1, Context.fetch!(Trace)} end)

    service2 =
      Filter.new()
      |> Filter.into(Trace.into())
      |> Filter.into(service1)

    Trace.bind(1, 2, 3, [], fn ->
      assert {2, trace} = Service.call(service2, 1)
      assert %Trace{trace_id: 1, parent_id: 3, span_id: span_id, flags: []} = trace
      assert span_id !== 3
    end)
  end
end
