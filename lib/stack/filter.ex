defmodule Stack.Filter do
  alias Stack.{Service, Filter}

  defstruct stack: []

  @dialyzer {:no_opaque, into: 2}
  @opaque t(_req_in, _rep_out, _req_out, _rep_in) :: %Filter{}

  @spec new() :: t(req, rep, req, rep) when req: var, rep: var
  def new(), do: %Filter{}

  @spec into(t(req_in, rep_out, req_out, rep_in), (req_out, (req -> rep) -> rep_in)) ::
          t(req_in, rep_out, req, rep)
        when req_in: var, rep_out: var, req_out: var, rep_in: var, req: var, rep: var
  def into(%Filter{stack: stack} = f, transformer) when is_function(transformer, 2) do
    %Filter{f | stack: [transformer | stack]}
  end

  @spec into(t(req_in, rep_out, req_out, rep_in), t(req_out, rep_in, req, rep)) ::
          t(req_in, rep_out, req, rep)
        when req_in: var, rep_out: var, req_out: var, rep_in: var, req: var, rep: var
  def into(%Filter{stack: stack1} = f, %Filter{stack: stack2}) do
    %Filter{f | stack: stack2 ++ stack1}
  end

  @spec into(t(req_in, rep_out, req_out, rep_in), Service.t(req_out, rep_in)) ::
          Service.t(req_in, rep_out)
        when req_in: var, rep_out: var, req_out: var, rep_in: var
  def into(%Filter{stack: stack}, %Service{} = s) do
    Enum.reduce(stack, s, &Service.wrap(&2, &1))
  end
end
