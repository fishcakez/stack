defmodule Stack.CacheTest do
  alias Stack.{Cache, CacheService, CacheTest, Service, Filter}
  use ExUnit.Case, async: true

  setup context do
    on_exit(fn -> assert Cache.delete(context.test) == true end)
  end

  test "map transforms values", context do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)
      |> Service.map(fn n -> n * 2 end)

    Cache.put(context.test, service)
    assert Cache.has_key?(context.test)
    cache_service = CacheService.new(context.test)

    assert Service.init(cache_service).(1) == 4
    assert Service.init(cache_service).(3) == 8
  end

  test "callback module transforms values", context do
    defmodule TestService do
      @behaviour Service

      @impl Service
      def init(n), do: n + 1

      @impl Service
      def call(n, m), do: n * m
    end

    service = Service.new(TestService, 1)
    Cache.put(context.test, service)
    cache_service = CacheService.new(context.test)

    assert Service.init(cache_service).(1) == 2
    assert Service.init(cache_service).(3) == 6
  end

  test "into places service inside a filter", context do
    service1 =
      Service.new()
      |> Service.map(fn n -> n + 1 end)

    service2 =
      Filter.new()
      |> Filter.transform(fn n, plus_one -> div(plus_one.(n), 2) end)
      |> Filter.transform(fn n, plus_one_div_two -> plus_one_div_two.(n * 3) end)
      |> Filter.into(service1)

    Cache.put(context.test, service2)
    cache_service = CacheService.new(context.test)

    assert Service.init(cache_service).(5) == 8
  end

  test "into with callback module filter places inside call", context do
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

    Cache.put(context.test, service2)
    cache_service = CacheService.new(context.test)

    assert Service.init(cache_service).(3) == 7
  end

  test "handle rescues exception with stacktrace", context do
    service =
      Filter.new()
      |> Filter.handle(fn req, err, stack -> {:error, req, err, stack} end)
      |> Filter.into(Service.new(fn _ -> raise RuntimeError end))

    Cache.put(context.test, service)
    cache_service = CacheService.new(context.test)

    assert {:error, 1, %RuntimeError{}, [{CacheTest, _, _, _} | _]} =
             Service.init(cache_service).(1)
  end

  test "ensure always runs", context do
    service =
      Filter.new()
      |> Filter.defer(fn req -> {self(), req} end, fn {pid, req} ->
        send(pid, {:deferred, req})
      end)
      |> Filter.handle(fn _, err, _ -> {:error, err} end)
      |> Filter.into(Service.new(fn _ -> raise RuntimeError end))

    Cache.put(context.test, service)
    cache_service = CacheService.new(context.test)

    assert {:error, %RuntimeError{}} = Service.init(cache_service).(1)
    assert_received {:deferred, 1}
  end

  test "delete removes cache entry", context do
    service =
      Service.new()
      |> Service.map(fn n -> n + 1 end)
      |> Service.map(fn n -> n * 2 end)

    Cache.put(context.test, service)
    cache_service = CacheService.new(context.test)

    assert Service.init(cache_service).(1) == 4
    Cache.delete(context.test)
    refute Cache.has_key?(context.test)
    assert Cache.fetch(context.test) == :error

    assert_raise KeyError, ~r/key :"test delete removes cache entry" not found in: %{.*}/, fn ->
      Service.init(cache_service).(3)
    end

    Cache.put(context.test, service)
    assert Service.init(cache_service).(3) == 8
  end

  test "deleting entry does not break or change explictly fetched service", context do
    service1 = Service.new(fn n -> n + 1 end)
    service2 = Service.new(fn n -> n + 2 end)

    Cache.put(context.test, service1)
    assert {:ok, cache_service} = Cache.fetch(context.test)

    assert Service.init(cache_service).(1) == 2
    Cache.delete(context.test)
    refute Cache.has_key?(context.test)
    assert Service.init(cache_service).(2) == 3
    Cache.put(context.test, service2)
    assert Service.init(cache_service).(3) == 4
  end

  test "put replaces cache entry", context do
    service1 = Service.new(fn n -> n + 1 end)
    service2 = Service.new(fn n -> n + 2 end)

    Cache.put(context.test, service1)
    cache_service = CacheService.new(context.test)

    assert Service.init(cache_service).(1) == 2
    Cache.put(context.test, service2)
    assert Service.init(cache_service).(1) == 3
  end
end
