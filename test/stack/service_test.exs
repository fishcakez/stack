defmodule Stack.ServiceTest do
  alias Stack.{Service, ServiceTest}
  use ExUnit.Case

  test "map transforms values" do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)
      |> Service.map(fn n -> n * 2 end, fn _ -> :error end)

    assert Service.init(service).(1) == 4
    assert Service.init(service).(3) == 8
  end

  test "map rescues exception" do
    service =
      Service.new()
      |> Service.map(fn _ -> raise RuntimeError end)
      |> Service.map(fn n -> n + 1 end, fn err -> {:error, err} end)

    assert {:error, %RuntimeError{}} = Service.init(service).(1)
  end

  test "map rescues exception with stacktrace" do
    service =
      Service.new()
      |> Service.map(fn _ -> raise RuntimeError end)
      |> Service.map(fn n -> n + 1 end, fn err, stack -> {:error, err, stack} end)

    assert {:error, %RuntimeError{}, [{ServiceTest, _, _, _} | _]} = Service.init(service).(1)
  end

  test "handle rescues exception" do
    service =
      Service.new()
      |> Service.map(fn _ -> raise RuntimeError end)
      |> Service.handle(fn err -> {:error, err} end)

    assert {:error, %RuntimeError{}} = Service.init(service).(1)
  end

  test "handle rescues exception with stacktrace" do
    service =
      Service.new()
      |> Service.map(fn _ -> raise RuntimeError end)
      |> Service.handle(fn err, stack -> {:error, err, stack} end)

    assert {:error, %RuntimeError{}, [{ServiceTest, _, _, _} | _]} = Service.init(service).(1)
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
    service = Service.append(service1, service2)

    assert Service.init(service).(1) == 4
  end

  test "append runs first service, maps value and then second" do
    service1 =
      Service.new()
      |> Service.map(fn n -> n + 1 end)

    service2 =
      Service.new()
      |> Service.map(fn n -> n * 3 end)

    service = Service.append(service1, service2, fn n -> div(n, 2) end)

    assert Service.init(service).(5) == 9
  end

  test "ensure always runs" do
    service =
      Service.new()
      |> Service.map(fn _ -> raise RuntimeError end)
      |> Service.ensure(fn -> send(self(), :ensured) end)
      |> Service.handle(fn err -> {:error, err} end)

    assert {:error, %RuntimeError{}} = Service.init(service).(1)
    assert_received :ensured
  end

  test "into places service inside a new service" do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)
      |> Service.into(fn n, plus_one -> plus_one.(n * 2) end)

    assert Service.init(service).(1) == 3
  end
end
