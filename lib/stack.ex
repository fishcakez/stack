defmodule Stack do
  @moduledoc false

  @typedoc false
  @type t(_req, _rep) :: [entry]

  @typedoc false
  @type entry ::
          {:map, (term -> term)}
          | {:into, (term, (term -> term) -> term)}
          | {:map | :into, module, term}

  @doc false
  @spec eval(t(req, rep), req) :: rep when req: var, rep: var
  def eval([], req), do: req
  def eval([{:into, transformer} | stack], req), do: transformer.(req, &eval(stack, &1))

  def eval([{:map, mapper} | stack], req) do
    stack
    |> eval(req)
    |> mapper.()
  end

  def eval([{:into, module, state} | stack], req) do
    module.call(req, &eval(stack, &1), state)
  end

  def eval([{:map, module, state} | stack], req) do
    stack
    |> eval(req)
    |> module.call(state)
  end

  @doc false
  @spec eval(t(req, rep), req, (term -> term)) :: rep when req: var, rep: var
  def eval([], req, service), do: service.(req)

  def eval([{:into, transformer} | stack], req, service) do
    transformer.(req, &eval(stack, &1, service))
  end

  def eval([{:map, mapper} | stack], req, service) do
    eval(stack, mapper.(req), service)
  end

  def eval([{:into, module, state} | stack], req, service) do
    module.call(req, &eval(stack, &1, service), state)
  end

  def eval([{:map, module, state} | stack], req, service) do
    eval(stack, module.call(req, state), service)
  end
end
