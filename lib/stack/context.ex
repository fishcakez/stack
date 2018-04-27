defmodule Stack.Context do
  @moduledoc """
  Context for a request.

  This module defines binding and reading operations for a immutable context. Access to a
  the values exists in the scope of the anonymous function for the calling process. Values
  should only be bound to the module that uses the context directly.
  """

  alias Stack.Context

  @typedoc """
  Context map of modules to bound values.
  """
  @type t :: %{optional(module) => term}

  @doc """
  Bind a context map in the scope of a fun, and run the fun.

  ## Examples

      iex> Stack.Context.bind(%{MyMod => "hi"}, fn -> Stack.Context.get(MyMod) end)
      "hi"
  """
  @spec bind(t, (() -> result)) :: result when result: var
  def bind(ctx, fun) when ctx == %{}, do: fun.()

  def bind(ctx, fun) when is_map(ctx) do
    case :erlang.get(Context) do
      %{} = old_ctx ->
        old_ctx
        |> Map.merge(ctx)
        |> bind_put(fun, old_ctx)

      _ ->
        bind_delete(ctx, fun)
    end
  end

  @doc """
  Bind a module's value in the scope of a fun, and run the fun.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.get(MyMod) end)
      "hi"
  """
  @spec bind(module, term, (() -> result)) :: result when result: var
  def bind(module, term, fun) when is_atom(module) do
    case :erlang.get(Context) do
      %{} = ctx ->
        ctx
        |> Map.put(module, term)
        |> bind_put(fun, ctx)

      _ ->
        bind_delete(%{module => term}, fun)
    end
  end

  @doc """
  Unbind all module's values in the scope of fun, and run the fun.

  ## Examples

      iex> Stack.Context.unbind(fn -> Stack.Context.get(MyMod) end)
      nil
  """
  @spec unbind((() -> result)) :: result when result: var
  def unbind(fun) do
    case :erlang.erase(Context) do
      %{} = old_ctx ->
        bind_put(fun, old_ctx)

      _ ->
        fun.()
    end
  end

  @doc """
  Unbind a module's value or list of modules' values in the scope of fun, and run the fun.

  ## Examples

      iex> Stack.Context.unbind(MyMod, fn -> Stack.Context.get(MyMod) end)
      nil
  """
  @spec unbind([module] | module, (() -> result)) :: result when result: var
  def unbind([], fun), do: fun.()

  def unbind([_ | _] = modules, fun) do
    case :erlang.get(Context) do
      %{} = old_ctx ->
        old_ctx
        |> Map.drop(modules)
        |> bind_put(fun, old_ctx)

      _ ->
        fun.()
    end
  end

  def unbind(module, fun) when is_atom(module) do
    case :erlang.get(Context) do
      %{} = old_ctx ->
        old_ctx
        |> Map.delete(module)
        |> bind_put(fun, old_ctx)

      _ ->
        fun.()
    end
  end

  @doc """
  Get the context map in the current scope.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.get() end)
      %{MyMod => "hi"}
  """
  @spec get() :: t
  def get() do
    case :erlang.get(Context) do
      %{} = ctx ->
        ctx

      _ ->
        %{}
    end
  end

  @doc """
  Get the modules' value bound in the current scope.

  If a value isn't bound to the module in current scope returns `nil`.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.get(MyMod) end)
      "hi"
      iex> Stack.Context.get(NotMyMod)
      nil
  """
  @spec get(module, term) :: term
  def get(module, default \\ nil) do
    case :erlang.get(Context) do
      %{^module => value} ->
        value

      _ ->
        default
    end
  end

  @doc """
  Fetches the module's value bound in the current scope and returns it in a tuple.

  If a value isn't bound to the module returns `:error`.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.fetch(MyMod) end)
      {:ok, "hi"}
      iex> Stack.Context.fetch(NotMyMod)
      :error
  """
  @spec fetch(module) :: {:ok, term} | :error
  def fetch(module) do
    case :erlang.get(Context) do
      %{^module => value} ->
        {:ok, value}

      _ ->
        :error
    end
  end

  @doc """
  Fetches the module's value bound in the current scope.

  If a value isn't bound to the module a `KeyError` is raised.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.fetch!(MyMod) end)
      "hi"
      iex> Stack.Context.fetch!(NotMyMod)
      ** (KeyError) key NotMyMod not found
  """
  @spec fetch!(module) :: term
  def fetch!(module) do
    case :erlang.get(Context) do
      %{^module => value} ->
        value

      _ ->
        raise KeyError, key: module
    end
  end

  @doc """
  Returns whether a module has a bound value in the current scope.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.has_key?(MyMod) end)
      true
      iex> Stack.Context.has_key?(NotMyMod)
      false
  """
  @spec has_key?(module) :: boolean
  def has_key?(module) do
    case :erlang.get(Context) do
      %{^module => _} ->
        true

      _ ->
        false
    end
  end

  @doc """
  Returns a list of modules with bound values in the current scope.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.keys() end)
      [MyMod]
  """
  @spec keys() :: [module]
  def keys() do
    case :erlang.get(Context) do
      %{} = ctx ->
        Map.keys(ctx)

      _ ->
        []
    end
  end

  ## Helpers

  defp bind_put(ctx, fun, old_ctx) do
    _ = :erlang.put(Context, ctx)

    try do
      fun.()
    after
      _ = :erlang.put(Context, old_ctx)
    end
  end

  defp bind_put(fun, old_ctx) do
    fun.()
  after
    _ = :erlang.put(Context, old_ctx)
  end

  defp bind_delete(ctx, fun) do
    _ = :erlang.put(Context, ctx)

    try do
      fun.()
    after
      _ = :erlang.erase(Context)
    end
  end
end
