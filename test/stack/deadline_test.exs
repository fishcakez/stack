defmodule Stack.DeadlineTest do
  alias Stack.{Deadline, Service, Filter}
  use ExUnit.Case, async: true

  test "timeout returns a timeout value at or before the deadline" do
    Deadline.bind(Deadline.new(1000), fn ->
      assert Deadline.timeout() <= 1000
    end)
  end

  test "start timer starts a timer that expires at or before the deadline" do
    Deadline.bind(Deadline.new(1000), fn ->
      timer = Deadline.start_timer(self(), :hello)
      assert Process.read_timer(timer) <= 1000
    end)
  end

  test "nested deadline merged with prior deadline" do
    Deadline.bind(Deadline.new(1000), fn ->
      Deadline.bind(Deadline.new(60000), fn ->
        assert Deadline.timeout() <= 1000
      end)
    end)

    Deadline.bind(Deadline.new(60000), fn ->
      Deadline.bind(Deadline.new(1000), fn ->
        assert Deadline.timeout() <= 1000
      end)
    end)
  end

  test "timeout returns timeout with into filter" do
    service1 =
      Service.new()
      |> Service.map(fn n -> {n + 1, Deadline.timeout()} end)

    service2 =
      Filter.new()
      |> Filter.into(Deadline.filter(1000))
      |> Filter.into(service1)

    assert {2, timeout} = Service.init(service2).(1)
    assert timeout <= 1000
  end
end
