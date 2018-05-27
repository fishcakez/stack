defmodule Stack.ConcurrencyTest do
  alias Stack.{ConcurrencyFilter, Failure, Criticality, Service, Filter}
  use ExUnit.Case, async: true

  defmodule Regulator do
    def start_link(args) do
      :sregulator.start_link(__MODULE__, args, [])
    end

    def init({queue, valve}) do
      queue_spec = {:sbroker_drop_queue, queue}
      valve_spec = {:sregulator_open_valve, valve}
      {:ok, {queue_spec, valve_spec, []}}
    end
  end

  setup context do
    {:ok, regulator} = Regulator.start_link({context[:queue] || %{}, context[:valve] || %{}})
    {:ok, [regulator: regulator]}
  end

  @tag queue: %{max: 0}
  @tag valve: %{max: 0}
  test "filter raises exception when dropped", context do
    service =
      ConcurrencyFilter.new(context[:regulator])
      |> Filter.into(Service.new(fn _ -> :ok end))
      |> Service.init()

    assert_raise Failure, ~r"dropped", fn -> service.(1) end
  end

  @tag queue: %{max: 1, drop: :drop}
  @tag valve: %{max: 1}
  test "filter blocks awaiting lock at threshold", context do
    service =
      ConcurrencyFilter.new(context[:regulator], [block_threshold: :sheddable_plus])
      |> Filter.into(Service.new(fn _ -> :ok end))
      |> Service.init()

    parent = self()
    spawn_link(fn ->
      {:go, ref, pid, _, _} = :sregulator.ask(context[:regulator])
      {:await, tag, _} = :sregulator.dynamic_ask(pid)
      send(parent, :go)
      assert_receive {^tag, {:drop, _}}
      # Test request entered queue and caused tag to be dropped, release lock to run test
      :sregulator.done(pid, ref)
      send(parent, :done)
    end)

    assert_receive :go

    assert Criticality.bind(:sheddable_plus, fn -> service.(1) end) == :ok
    assert_receive :done
  end

  @tag queue: %{max: 1}
  @tag valve: %{max: 0}
  test "filter does not block awaiting lock below threshold", context do
    service =
      ConcurrencyFilter.new(context[:regulator], [block_threshold: :critical_plus])
      |> Filter.into(Service.new(fn _ -> :ok end))
      |> Service.init()

    assert_raise Failure, ~r"dropped", fn -> service.(1) end
  end

  @tag queue: %{max: 0}
  @tag valve: %{max: 1}
  test "filter release lock after service runs", context do
    service =
      ConcurrencyFilter.new(context[:regulator])
      |> Filter.into(Service.new(fn _ -> :ok end))
      |> Service.init()

    assert service.(1) == :ok
    assert service.(2) == :ok
  end


  @tag queue: %{max: 0}
  @tag valve: %{max: 1}
  test "filter release lock after service raises", context do
    service =
      ConcurrencyFilter.new(context[:regulator])
      |> Filter.into(Service.new(fn _ -> raise "oops" end))
      |> Service.init()

    assert_raise RuntimeError, "oops", fn -> service.(1) end
    assert_raise RuntimeError, "oops", fn -> service.(2) end
  end

end