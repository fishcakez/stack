defmodule Stack.Context do
  alias Stack.Context

  @type t :: %{optional(module) => term}

  @spec bind(t, (() -> result)) :: result when result: var
  def bind(ctx, fun) when is_map(ctx) do
    bind(ctx, fun, [])
  end

  @spec bind(t, (... -> result), list) :: result when result: var
  def bind(ctx, fun, args) when ctx == %{}, do: apply(fun, args)

  def bind(ctx, fun, args) when is_map(ctx) do
    case :erlang.get(Context) do
      %{} = old_ctx ->
        old_ctx
        |> Map.merge(ctx)
        |> bind_put(fun, args, old_ctx)

      _ ->
        bind_delete(ctx, fun, args)
    end
  end

  @spec bind(module, term, (() -> result)) :: result when result: var
  def bind(module, term, fun) when is_atom(module) do
    bind(module, term, fun, [])
  end

  @spec bind(module, term, (... -> result), list) :: result when result: var
  def bind(module, term, fun, args \\ []) when is_atom(module) do
    case :erlang.get(Context) do
      %{} = ctx ->
        ctx
        |> Map.put(module, term)
        |> bind_put(fun, args, ctx)

      _ ->
        bind_delete(%{module => term}, fun, args)
    end
  end

  @spec unbind((() -> result)) :: result when result: var
  def unbind(fun) when is_function(fun) do
    unbind(fun, [])
  end

  @spec unbind((... -> result), list) :: result when result: var
  def unbind(fun, args) when is_function(fun) do
    case :erlang.erase(Context) do
      %{} = old_ctx ->
        bind_put(fun, args, old_ctx)

      _ ->
        fun.()
    end
  end

  @spec unbind([module] | module, (() -> result)) :: result when result: var
  def unbind(modules, fun) when is_atom(modules) or is_list(modules) do
    unbind(modules, fun, [])
  end

  @spec unbind([module] | module, (... -> result), list) :: result when result: var
  def unbind([], fun, args), do: apply(fun, args)

  def unbind([_ | _] = modules, fun, args) do
    case :erlang.get(Context) do
      %{} = old_ctx ->
        old_ctx
        |> Map.drop(modules)
        |> bind_put(fun, args, old_ctx)

      _ ->
        apply(fun, args)
    end
  end

  def unbind(module, fun, args) when is_atom(module) do
    case :erlang.get(Context) do
      %{} = old_ctx ->
        old_ctx
        |> Map.delete(module)
        |> bind_put(fun, args, old_ctx)

      _ ->
        apply(fun, args)
    end
  end

  @spec get() :: t
  def get() do
    case :erlang.get(Context) do
      %{} = ctx ->
        ctx

      _ ->
        %{}
    end
  end

  @spec get(module, term) :: term
  def get(module, default \\ nil) do
    case :erlang.get(Context) do
      %{^module => value} ->
        value

      _ ->
        default
    end
  end

  @spec fetch(module) :: {:ok, term} | :error
  def fetch(module) do
    case :erlang.get(Context) do
      %{^module => value} ->
        {:ok, value}

      _ ->
        :error
    end
  end

  @spec fetch!(module) :: term
  def fetch!(module) do
    case :erlang.get(Context) do
      %{^module => value} ->
        value

      _ ->
        raise KeyError, key: module
    end
  end

  @spec has_key?(module) :: boolean
  def has_key?(module) do
    case :erlang.get(Context) do
      %{^module => _} ->
        true

      _ ->
        false
    end
  end

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

  defp bind_put(ctx, fun, args, old_ctx) do
    _ = :erlang.put(Context, ctx)

    try do
      apply(fun, args)
    after
      _ = :erlang.put(Context, old_ctx)
    end
  end

  defp bind_put(fun, args, old_ctx) do
    apply(fun, args)
  after
    _ = :erlang.put(Context, old_ctx)
  end

  defp bind_delete(ctx, fun, args) do
    _ = :erlang.put(Context, ctx)

    try do
      apply(fun, args)
    after
      _ = :erlang.erase(Context)
    end
  end
end
