defmodule Stack.Cache do
  @moduledoc """
  Cache for `Stack.Service`.

  A singleton cache for `Stack.Service` datastructures. The cache uses compiled modules and ETS
  to optimize a cached `Stack.Service` by unrolling the stack and reducing copying of terms.
  """
  alias Stack.{Cache, Service}
  use GenServer

  @typedoc """
  A key to the cache.

  A key must be an atom.
  """
  @type key :: atom

  @doc """
  Associate the given `Stack.Service.t` with the `key`.
  """
  @spec put(key, Service.t(_req, _rep)) :: :ok when _req: var, _rep: var
  def put(key, service) when is_atom(key) do
    {quoted, values} = quoted(service)
    GenServer.call(Cache, {:put, key, quoted, values})
  end

  @doc """
  Delete the given `Stack.Service.t` associated with the `key`.

  Returns `true` if successful, otherwise `false`.
  """
  @spec delete(key) :: boolean
  def delete(key) do
    GenServer.call(Cache, {:delete, key})
  end

  @doc """
  Fetch the `Stack.Service.t` associated with the `key`.

  Returns `{:ok, service}` if a service is associated with the `key`, otherwise `:error`.
  """
  @spec fetch(key) :: {:ok, Service.t(_req, _rep)} | :error when _req: var, _rep: var
  def fetch(key) do
    :ets.lookup_element(Cache, key, 2)
  rescue
    ArgumentError ->
      :error
  else
    service ->
      {:ok, service}
  end

  @doc """
  Fetch the `Stack.Service.t` associated with the `key`.

  Returns `service` if a service is associated with the `key`, otherwise raises `KeyError`.
  """
  @spec fetch!(key) :: Service.t(_req, _rep) when _req: var, _rep: var
  def fetch!(key) do
    :ets.lookup_element(Cache, key, 2)
  rescue
    ArgumentError ->
      case Cache |> :ets.tab2list() |> Map.new() do
        %{^key => service} ->
          # Handle race condition where entry is added after miss.
          service

        %{} = map ->
          raise KeyError, key: key, term: map
      end
  end

  @doc """
  Validates whether a `Stack.Service.t` is associated with the `key`.

  Returns `true` if a service is associated with the `key`, otherwise `false`.
  """
  @spec has_key?(key) :: boolean
  def has_key?(key) do
    :ets.member(Cache, key)
  end

  @doc false
  def start_link(opts) do
    GenServer.start_link(Cache, Cache, [name: Cache] ++ opts)
  end

  @impl GenServer
  def init(tab) do
    ^tab = :ets.new(tab, [:named_table, :public, :ordered_set, {:read_concurrency, true}])
    {:ok, :ets.whereis(tab)}
  end

  @impl GenServer
  def handle_call({:put, key, quoted, values}, _from, tab) do
    if :ets.member(tab, key) do
      delete_fun(key)
    end

    fun = make_fun(key, quoted)
    :ets.insert(tab, {key, Service.new(Cache.Service, {fun, values})})
    {:reply, :ok, tab}
  end

  def handle_call({:delete, key}, _from, tab) do
    {:reply, delete_fun(key) and :ets.delete(tab, key), tab}
  end

  defp delete_fun(key) do
    module = Module.concat(Cache, key)
    :code.soft_purge(module) and :code.delete(module)
  end

  defp make_fun(key, quoted) do
    module = Module.concat(Cache, key)
    {:module, ^module, _, _} = Module.create(module, quoted, Macro.Env.location(__ENV__))
    module.__init__()
  end

  defp quoted(%Service{stack: stack}) do
    req = Macro.var(:req, __MODULE__)
    {expr, binding} = Stack.quoted(stack, req)
    {vars, values} = Enum.unzip(binding)

    quoted =
      quote location: :keep, generated: true do
        def __init__() do
          # local fun so still works if module deleted (but not purged)
          &call/2
        end

        defp call(unquote(req), {unquote_splicing(vars)}) do
          unquote(expr)
        end
      end

    {quoted, List.to_tuple(values)}
  end
end
