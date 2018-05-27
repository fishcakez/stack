defmodule Stack.FailureTest do
  alias Stack.{FailureFilter, Failure, Service, Filter}
  use ExUnit.Case, async: true

  test "retries up to 3 times" do
    service =
      FailureFilter.new(fn :oops -> :retry end)
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, FailureFilter.tries()) end)
      |> Filter.into(Service.new(fn _ -> :oops end))
      |> Service.init()

    assert_raise Failure, fn -> service.(1) end

    assert_received 1
    assert_received 2
    assert_received 3
    refute_received _4
  end

  test "does not retry if exception doesn't match policy" do
    service =
      FailureFilter.new(fn :oops -> :error end)
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, FailureFilter.tries()) end)
      |> Filter.into(Service.new(fn _ -> :oops end))
      |> Service.init()

    assert_raise Failure, fn -> service.(1) end
    assert_received 1
    refute_received _2
  end

  test "backoff is longer than the backoff 2 tries ago" do
    service =
      FailureFilter.new(fn :oops -> :reject end, max_tries: 5, backoff: :backoff.init(50, 500))
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, System.monotonic_time(1000)) end)
      |> Filter.into(Service.new(fn _ -> :oops end))
      |> Service.init()

    assert_raise Failure, fn -> service.(1) end

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
      FailureFilter.new(fn :oops -> :retry end, backoff: :backoff.init(1, 1))
      |> Filter.defer(fn _ -> self() end, fn pid -> send(pid, FailureFilter.tries()) end)
      |> Filter.into(Service.new(fn _ -> :oops end))
      |> Service.init()

    assert_raise Failure, fn -> service.(1) end

    assert_received 1
    assert_received 2
    assert_received 3
    refute_received _4
  end
end
