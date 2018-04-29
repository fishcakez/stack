defmodule Stack.Service do
  @moduledoc """
  A builder for composing a stack based service.

  A service creates a stack of functions that transform an input request to an
  output reply. A fun that maps the request to the reply is created with `init/1`.
  When building the service stack, the request can not be changed but the response
  can be unless the service is wrapped with `into/2` or by a `Filter.t`.
  """
  alias Stack.Service

  defstruct stack: []

  @typedoc """
  A service with request and reply parameters.

  The first parameter is request, or input, to the service.
  The second parameter is reply, our output, to the service.
  """
  @opaque t(req, rep) :: %Service{stack: Stack.t(req, rep)}

  @callback init(args) :: state when args: term, state: term
  @callback call(_req, state) :: _rep when _req: var, state: term, _rep: var

  @doc """
  Create a new (identity) service.
  """
  @spec new() :: t(req, req) when req: var
  def new(), do: %Service{}

  @doc """
  Create a new service with a fun.

  The service applies the fun to input and returns the output.
  """
  @spec new((req -> rep)) :: t(req, rep) when req: var, rep: var
  def new(mapper) when is_function(mapper, 1) do
    %Service{stack: [{:map, mapper}]}
  end

  @doc """
  Create a new service with a callback module and args.

  The service calls the callback module with the input and returns the output.
  """
  @spec new(module, args) :: t(_req, _rep) when args: term, _req: var, _rep: var
  def new(module, args) when is_atom(module) do
    state = module.init(args)
    %Service{stack: [{:map, module, state}]}
  end

  @doc """
  Extends the service to map the current reply to a new result.
  """
  @spec map(t(req, rep), (rep -> res)) :: t(req, res) when req: var, rep: var, res: var
  def map(%Service{stack: stack} = s, mapper) when is_function(mapper, 1) do
    %Service{s | stack: [{:map, mapper} | stack]}
  end

  @doc """
  Extends a service with another service so that the output of first is input to second.
  """
  @spec map(t(req, rep), t(rep, res)) :: t(req, res) when req: var, rep: var, res: var
  def map(%Service{stack: stack1} = s, %Service{stack: stack2}) do
    %Service{s | stack: stack2 ++ stack1}
  end

  @doc """
  Extends the service to run a fun on the current reply, ignoring the result.
  """
  @spec each(t(req, rep), (req -> any)) :: t(req, rep) when req: var, rep: var
  def each(%Service{} = s, runner) when is_function(runner, 1) do
    map(s, fn rep ->
      _ = runner.(rep)
      rep
    end)
  end

  @doc """
  Create an anonymous function that transforms the input to ouput.
  """
  @spec init(t(req, rep)) :: (req -> rep) when req: var, rep: var
  def init(%Service{stack: stack}) do
    &Stack.eval(stack, &1)
  end

  @doc """
  Call a service with the input.
  """
  @spec call(t(req, rep), req) :: rep when req: var, rep: var
  def call(%Service{stack: stack}, req) do
    Stack.eval(stack, req)
  end
end
