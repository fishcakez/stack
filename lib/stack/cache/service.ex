defmodule Stack.Cache.Service do
  @moduledoc false
  alias Stack.Service
  @behaviour Service

  @impl Service
  def init({fun, arg} = fun_arg) when is_function(fun, 2) and is_tuple(arg) do
    fun_arg
  end

  @impl Service
  def call(req, {fun, arg}) do
    fun.(req, arg)
  end
end
