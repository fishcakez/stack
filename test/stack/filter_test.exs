defmodule Stack.FilterTest do
  alias Stack.{Service, Filter}
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

    assert Service.call(service2, 5) == 8
  end
end
