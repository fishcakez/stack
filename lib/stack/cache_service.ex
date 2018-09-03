defmodule Stack.CacheService do
  @moduledoc """
  Cached `Stack.Service`.

  This service fetches the service from `Stack.Cache` on every request and calls it. It is useful
  to avoid passing around a service datastructure and takes advantage of the optimizations provided
  by `Stack.Cache`. The service can also be updated outside of the logic using the service.
  """
  alias Stack.{Cache, Service}
  @behaviour Service

  @doc """
  Create a service that calls a cached service via `Stack.Cache`.

  ## Examples

      Stack.Cache.put(:foo, service)
      Stack.CacheService.new(:foo)

  The cached service is fetched on every call so any changes to the cache are automatically used.
  """
  @spec new(Cache.key()) :: Service.t(_req, _rep) when _req: var, _rep: var
  def new(key) when is_atom(key) do
    Service.new(__MODULE__, key)
  end

  @impl Service
  def init(key) do
    key
  end

  @impl Service
  def call(req, key) do
    key
    |> Cache.fetch!()
    |> Service.call(req)
  end
end
