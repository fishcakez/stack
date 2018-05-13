defmodule Stack.RetryTest do
  alias Stack.{Retry, Service, Filter}
  use ExUnit.Case, async: true

  test "retries up to 3 times" do
    service =
      Retry.filter(fn %exception{} -> exception == RuntimeError end)
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, Retry.tries()) end)
      |> Filter.into(Service.new(fn _ -> raise "oops" end))
      |> Service.init()

    assert_raise RuntimeError, "oops", fn -> service.(1) end

    assert_received 1
    assert_received 2
    assert_received 3
    refute_received _4
  end

  test "does not retry if exception doesn't match policy" do
    service =
      Retry.filter(fn _ -> false end)
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, Retry.tries()) end)
      |> Filter.into(Service.new(fn _ -> raise "oops" end))
      |> Service.init()

    assert_raise RuntimeError, "oops", fn -> service.(1) end
    assert_received 1
    refute_received _2
  end

  test "backoff is longer than the backoff 2 tries ago" do
    service =
      Retry.filter(fn _ -> true end, max_tries: 5, max_backoff: 1000)
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, System.monotonic_time(1000)) end)
      |> Filter.into(Service.new(fn _ -> raise "oops" end))
      |> Service.init()

    assert_raise RuntimeError, "oops", fn -> service.(1) end

    assert_received first
    assert_received second
    assert_received third
    assert_received fourth
    assert second - first < fourth - third

    assert_received fifth
    assert third - second < fifth - fourth

    refute_received _sixth
  end

  test "max backoff is always 1" do
    service =
      Retry.filter(fn _ -> true end, max_backoff: 1)
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, Retry.tries()) end)
      |> Filter.into(Service.new(fn _ -> raise "oops" end))
      |> Service.init()

    assert_raise RuntimeError, "oops", fn -> service.(1) end

    assert_received 1
    assert_received 2
    assert_received 3
    refute_received _4
  end
end
