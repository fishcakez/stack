defmodule Stack.ContextTest do
  alias Stack.{Context, ContextTest}
  use ExUnit.Case, async: true

  doctest Stack.Context

  test "bind value sets value" do
    Context.bind(ContextTest, 1, fn ->
      assert Context.get() == %{ContextTest => 1}
    end)

    assert Context.get() == %{}
  end

  test "merge bindings in nested bind value" do
    Context.bind(ContextTest, 1, fn ->
      Context.bind(MyMod, 2, fn ->
        assert Context.get() == %{ContextTest => 1, MyMod => 2}
      end)

      assert Context.get() == %{ContextTest => 1}
    end)
  end

  test "bind context sets context" do
    Context.bind(%{ContextTest => 1}, fn ->
      assert Context.get() == %{ContextTest => 1}
    end)

    assert Context.get(ContextTest, :nope) == :nope
  end

  test "merge bindings in nested bind context" do
    Context.bind(%{ContextTest => 1}, fn ->
      Context.bind(%{MyMod => 2}, fn ->
        assert Context.get() == %{ContextTest => 1, MyMod => 2}
      end)

      assert Context.fetch!(ContextTest) == 1
    end)
  end

  test "unbind keys are unbound in nested context" do
    Context.bind(%{ContextTest => 1, MyMod => 2}, fn ->
      Context.unbind([ContextTest], fn ->
        assert Context.keys() == [MyMod]
      end)

      assert Context.get() == %{ContextTest => 1, MyMod => 2}
    end)
  end

  test "unbind full context is unbound in nested context" do
    Context.bind(%{ContextTest => 1}, fn ->
      Context.unbind(fn ->
        assert Context.keys() == []
      end)

      assert Context.get() == %{ContextTest => 1}
    end)
  end
end
