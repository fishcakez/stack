defmodule Stack.Context do
  @moduledoc """
  Context for a request.

  This module defines binding and reading operations for a immutable context. Access to a
  the values exists in the scope of the anonymous function for the calling process.
  """

  @typedoc """
  Context map.
  """
  @type t :: %{term => term}

  @doc """
  Bind a context map in the scope of a fun, and run the fun.

  ## Examples

      iex> Stack.Context.bind(%{MyMod => "hi"}, fn -> Stack.Context.get(MyMod) end)
      "hi"
  """
  @spec bind(t, (() -> result)) :: result when result: var
  def bind(ctx, fun) when is_map(ctx) do
    case get_process_metadata() do
      %{} = old_ctx ->
        old_ctx
        |> Map.merge(ctx)
        |> put(fun, old_ctx)

      _ ->
        put(ctx, fun)
    end
  end

  @doc """
  Bind a value in the scope of a fun, and run the fun.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.get(MyMod) end)
      "hi"
  """
  @spec bind(term, term, (() -> result)) :: result when result: var
  def bind(key, value, fun) do
    case get_process_metadata() do
      %{} = ctx ->
        ctx
        |> Map.put(key, value)
        |> put(fun, ctx)

      _ ->
        put(%{key => value}, fun)
    end
  end

  @doc """
  Bind a context map in the scope of a fun, run the fun and after run deferred fun.

  ## Examples

      iex> Stack.Context.bind_defer(%{MyMod => "hi"}, fn -> Stack.Context.get(MyMod) end, fn -> :deferred end)
      "hi"
  """
  @spec bind_defer(t, (() -> result), (() -> _ignore)) :: result when result: var, _ignore: var
  def bind_defer(ctx, fun, deferred) when is_map(ctx) do
    case get_process_metadata() do
      %{} = old_ctx ->
        old_ctx
        |> Map.merge(ctx)
        |> put_defer(fun, deferred, old_ctx)

      _ ->
        put_defer(ctx, fun, deferred)
    end
  end

  @doc """
  Bind a context map in the scope of a fun, run the fun and after run deferred fun.

  ## Examples

      iex> Stack.Context.bind_defer(%{MyMod => "hi"}, fn -> Stack.Context.get(MyMod) end, fn -> :deferred end)
      "hi"
  """
  @spec bind_defer(term, term, (() -> result), (() -> _ignore)) :: result
        when result: var, _ignore: var
  def bind_defer(key, value, fun, deferred) do
    case get_process_metadata() do
      %{} = ctx ->
        ctx
        |> Map.put(key, value)
        |> put_defer(fun, ctx, deferred)

      _ ->
        put_defer(%{key => value}, fun, deferred)
    end
  end

  @doc """
  Unbind all keys' values in the scope of fun, and run the fun.

  ## Examples

      iex> Stack.Context.unbind(fn -> Stack.Context.get(MyMod) end)
      nil
  """
  @spec unbind((() -> result)) :: result when result: var
  def unbind(fun) do
    case get_process_metadata() do
      %{} = old_ctx ->
        put(%{}, fun, old_ctx)

      _ ->
        fun.()
    end
  end

  @doc """
  Unbind a list of keys' values in the scope of fun, and run the fun.

  ## Examples

      iex> Stack.Context.unbind(MyMod, fn -> Stack.Context.get(MyMod) end)
      nil
  """
  @spec unbind([term], (() -> result)) :: result when result: var
  def unbind(keys, fun) do
    case get_process_metadata() do
      %{} = old_ctx ->
        old_ctx
        |> Map.drop(keys)
        |> put(fun, old_ctx)

      _ ->
        fun.()
    end
  end

  @doc """
  Unbind all keys' values in the scope of fun, run the fun, and after run deferred fun.

  ## Examples

      iex> Stack.Context.unbind_defer(fn -> Stack.Context.get(MyMod) end, fn -> :deferred end)
      nil
  """
  @spec unbind_defer((() -> result), (() -> _ignore)) :: result when result: var, _ignore: var
  def unbind_defer(fun, deferred) do
    case get_process_metadata() do
      %{} = old_ctx ->
        put_defer(%{}, fun, deferred, old_ctx)

      _ ->
        try do
          fun.()
        after
          deferred.()
        end
    end
  end

  @doc """
  Unbind a list of keys' values in the scope of fun, run the fun, and after run deferred fun.

  ## Examples

      iex> Stack.Context.unbind_defer([MyMod], fn -> Stack.Context.get(MyMod) end, fn -> :deferred end)
      nil
  """
  @spec unbind_defer([term], (() -> result), (() -> _ignore)) :: result
        when result: var, _ignore: var
  def unbind_defer(keys, fun, deferred) do
    case get_process_metadata() do
      %{} = old_ctx ->
        old_ctx
        |> Map.drop(keys)
        |> put_defer(fun, deferred, old_ctx)

      _ ->
        try do
          fun.()
        after
          deferred.()
        end
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
    case get_process_metadata() do
      %{} = ctx ->
        ctx

      _ ->
        %{}
    end
  end

  @doc """
  Get the keys' value bound in the current scope.

  If a value isn't bound to the key in current scope returns the default value.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.get(MyMod) end)
      "hi"
      iex> Stack.Context.get(NotMyMod)
      nil
  """
  @spec get(term, term) :: term
  def get(key, default \\ nil) do
    case get_process_metadata() do
      %{^key => value} ->
        value

      _ ->
        default
    end
  end

  @doc """
  Fetches the key's value bound in the current scope and returns it in a tuple.

  If a value isn't bound to the key returns `:error`.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.fetch(MyMod) end)
      {:ok, "hi"}
      iex> Stack.Context.fetch(NotMyMod)
      :error
  """
  @spec fetch(term) :: {:ok, term} | :error
  def fetch(key) do
    case get_process_metadata() do
      %{^key => value} ->
        {:ok, value}

      _ ->
        :error
    end
  end

  @doc """
  Fetches the key's value bound in the current scope.

  If a value isn't bound to the key a `KeyError` is raised.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.fetch!(MyMod) end)
      "hi"
      iex> Stack.Context.fetch!(NotMyMod)
      ** (KeyError) key NotMyMod not found
  """
  @spec fetch!(term) :: term
  def fetch!(key) do
    case get_process_metadata() do
      %{^key => value} ->
        value

      _ ->
        raise KeyError, key: key
    end
  end

  @doc """
  Returns whether a key has a bound value in the current scope.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.has_key?(MyMod) end)
      true
      iex> Stack.Context.has_key?(NotMyMod)
      false
  """
  @spec has_key?(term) :: boolean
  def has_key?(key) do
    case get_process_metadata() do
      %{^key => _} ->
        true

      _ ->
        false
    end
  end

  @doc """
  Returns a list of keys with bound values in the current scope.

  ## Examples

      iex> Stack.Context.bind(MyMod, "hi", fn -> Stack.Context.keys() end)
      [MyMod]
  """
  @spec keys() :: [term]
  def keys() do
    case get_process_metadata() do
      %{} = ctx ->
        Map.keys(ctx)

      _ ->
        []
    end
  end

  ## Helpers

  defp put(ctx, fun) do
    set_process_metadata(ctx)

    try do
      fun.()
    after
      unset_process_metadata()
    end
  end

  defp put(ctx, fun, old_ctx) do
    set_process_metadata(ctx)

    try do
      fun.()
    after
      set_process_metadata(old_ctx)
    end
  end

  defp put_defer(ctx, fun, deferred) do
    set_process_metadata(ctx)

    try do
      fun.()
    after
      unset_process_metadata()
      deferred.()
    end
  end

  defp put_defer(ctx, fun, deferred, old_ctx) do
    set_process_metadata(ctx)

    try do
      fun.()
    after
      set_process_metadata(old_ctx)
      deferred.()
    end
  end

  @compile {:inline, get_process_metadata: 0, set_process_metadata: 1, unset_process_metadata: 0}

  if function_exported?(:logger, :get_process_metadata, 0) do
    defp get_process_metadata(), do: :logger.get_process_metadata()
    defp set_process_metadata(ctx), do: :logger.set_process_metadata(ctx)
    defp unset_process_metadata(), do: :logger.unset_process_metadata()
  else
    def set_process_metadata(map) do
      _ = :erlang.put(Context, map)
      :ok
    end

    def get_process_metadata() do
      :erlang.get(Context)
    end

    def unset_process_metadata() do
      :erlang.erase(Context)
      :ok
    end
  end
end
