defmodule Stack.ServiceTest do
  alias Stack.Service
  use ExUnit.Case

  test "map transforms values" do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)
      |> Service.map(fn n -> n * 2 end)

    assert Service.init(service).(1) == 4
    assert Service.init(service).(3) == 8
  end

  test "callback module transforms values" do
    defmodule TestService do
      @behaviour Service

      @impl Service
      def init(n), do: n + 1

      @impl Service
      def call(n, m), do: n * m
    end

    service = Service.new(TestService, 1)
    assert Service.init(service).(1) == 2
    assert Service.init(service).(3) == 6
  end

  test "each runs fun without changing request" do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)
      |> Service.each(fn n -> send(self(), {:ran, n}) end)
      |> Service.map(fn n -> n * 2 end)

    assert Service.init(service).(1) == 4
    assert_received {:ran, 2}
  end

  test "append runs first service and then second" do
    service1 = Service.new(fn n -> n + 1 end)
    service2 = Service.new(fn n -> n * 2 end)
    service = Service.map(service1, service2)

    assert Service.init(service).(1) == 4
  end

  test "into places service inside a new service" do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)
      |> Service.into(fn n, plus_one -> plus_one.(n * 2) end)

    assert Service.init(service).(1) == 3
  end
end
