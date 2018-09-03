defmodule Stack do
  @moduledoc false

  @typedoc false
  @type t(_req, _rep) :: [entry]

  @typedoc false
  @type entry ::
          {:map, (term -> term)}
          | {:transform, (term, (term -> term) -> term)}
          | {:map | :transform, module, term}

  @doc false
  @spec eval(t(req, rep), req) :: rep when req: var, rep: var
  def eval([], req), do: req
  def eval([{:transform, transformer} | stack], req), do: transformer.(req, &eval(stack, &1))

  def eval([{:map, mapper} | stack], req) do
    stack
    |> eval(req)
    |> mapper.()
  end

  def eval([{:transform, module, state} | stack], req) do
    module.call(req, &eval(stack, &1), state)
  end

  def eval([{:map, module, state} | stack], req) do
    stack
    |> eval(req)
    |> module.call(state)
  end

  @doc false
  @spec quoted(t(term, term), Macro.t()) :: {Macro.t(), binding} when binding: [{Macro.t(), term}]
  def quoted([], req) do
    rep =
      quote do
        unquote(req)
      end

    {rep, []}
  end

  def quoted([{:transform, transformer} | stack], req_ex) do
    req_in = Macro.var(:"req#{length(stack)}", __MODULE__)
    {service, binding} = quoted(stack, req_in)
    {transformer, binding} = escape(transformer, binding)

    rep_ex =
      quote do
        unquote(transformer).(unquote(req_ex), fn unquote(req_in) -> unquote(service) end)
      end

    {rep_ex, binding}
  end

  def quoted([{:map, mapper} | stack], req) do
    {res, binding} = quoted(stack, req)
    {mapper, binding} = escape(mapper, binding)

    rep =
      quote do
        unquote(mapper).(unquote(res))
      end

    {rep, binding}
  end

  def quoted([{:transform, mod, args} | stack], req_ex) do
    req_in = Macro.var(:"req#{length(stack)}", __MODULE__)
    {service, binding} = quoted(stack, req_in)
    {args, binding} = escape(args, binding)

    rep_ex =
      quote do
        unquote(mod).call(
          unquote(req_ex),
          fn unquote(req_in) -> unquote(service) end,
          unquote(args)
        )
      end

    {rep_ex, binding}
  end

  def quoted([{:map, mod, args} | stack], req) do
    {res, binding} = quoted(stack, req)
    {args, binding} = escape(args, binding)

    rep =
      quote do
        unquote(mod).call(unquote(res), unquote(args))
      end

    {rep, binding}
  end

  defp escape(term, binding) do
    {Macro.escape(term), binding}
  rescue
    ArgumentError ->
      var = Macro.var(:"elem#{length(binding)}", __MODULE__)
      {var, [{var, term} | binding]}
  end
end
