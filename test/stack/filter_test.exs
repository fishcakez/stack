defmodule Stack.FilterTest do
  alias Stack.{Service, Filter, FilterTest}
  use ExUnit.Case, async: true

  test "into places service inside a filter" do
    service1 =
      Service.new()
      |> Service.map(fn n -> n + 1 end)

    service2 =
      Filter.new()
      |> Filter.into(fn n, plus_one -> div(plus_one.(n), 2) end)
      |> Filter.into(fn n, plus_one_div_two -> plus_one_div_two.(n * 3) end)
      |> Filter.into(service1)

    assert Service.init(service2).(5) == 8
  end

  test "into with callback module filter places inside call" do
    defmodule TestFilter do
      @behaviour Filter

      @impl Filter
      def init(n), do: n + 1

      @impl Filter
      def call(n, service, m), do: service.(n * m)
    end

    service1 =
      Service.new()
      |> Service.map(fn n -> n + 1 end)

    service2 =
      Filter.new(TestFilter, 1)
      |> Filter.into(service1)

    assert Service.init(service2).(3) == 7
  end

  test "init creates fun replicated filter" do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)

    filter =
      Filter.new()
      |> Filter.into(fn n, plus_one -> div(plus_one.(n), 2) end)
      |> Filter.into(fn n, plus_one_div_two -> plus_one_div_two.(n * 3) end)

    assert Filter.init(filter).(5, Service.init(service)) == 8
  end

  test "transform rescues exception with stacktrace" do
    service =
      Filter.transform(fn n -> n + 1 end, fn req, err, stack -> {:error, req, err, stack} end)
      |> Filter.into(Service.new(fn _ -> raise RuntimeError end))

    assert {:error, 1, %RuntimeError{}, [{FilterTest, _, _, _} | _]} = Service.init(service).(1)
  end

  test "handle rescues exception with stacktrace" do
    service =
      Filter.handle(fn req, err, stack -> {:error, req, err, stack} end)
      |> Filter.into(Service.new(fn _ -> raise RuntimeError end))

    assert {:error, 1, %RuntimeError{}, [{FilterTest, _, _, _} | _]} = Service.init(service).(1)
  end

  test "ensure always runs" do
    service =
      Filter.ensure(fn req -> send(self(), {:ensured, req}) end)
      |> Filter.into(Filter.handle(fn _, err, _ -> {:error, err} end))
      |> Filter.into(Service.new(fn _ -> raise RuntimeError end))

    assert {:error, %RuntimeError{}} = Service.init(service).(1)
    assert_received {:ensured, 1}
  end
end
