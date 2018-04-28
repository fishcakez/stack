defmodule Stack.Service do
  @moduledoc """
  A builder for composing a stack based service.

  A service creates a stack of functions that transform an input request to an
  output reply. A fun that maps the request to the reply is created with `init/1`.
  When building the service stack, the request can not be changed but the response
  can be unless the service is wrapped with `into/2` or by a `Filter.t`.

  Only fun's are supported so that the service can be typed when built, and dialyzer can
  analyze the composition with success typing.
  """
  alias Stack.Service

  defstruct stack: []

  @typedoc """
  A service with request and reply parameters.

  The first parameter is request, or input, to the service.
  The second parameter is reply, our output, to the service.
  """
  @opaque t(_req, _rep) :: %Service{}

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
  Extends the service to map the current reply to a new result.
  """
  @spec map(t(req, rep), (rep -> res)) :: t(req, res) when req: var, rep: var, res: var
  def map(%Service{stack: stack} = s, mapper) when is_function(mapper, 1) do
    %Service{s | stack: [{:map, mapper} | stack]}
  end

  @doc """
  Extends the service to map the current reply to a new result.

  If an exception is raised at the same point in the stack maps the exception to a new
  result instead. However if this map, or a function added later in the stack raises
  it won't be rescued.
  """
  @spec map(t(req, rep), (rep -> res), (Exception.t() -> res)) :: t(req, res)
        when req: var, rep: var, res: var
  def map(%Service{stack: stack} = s, mapper, handler)
      when is_function(mapper, 1) and is_function(handler, 1) do
    %Service{s | stack: [{:map, mapper, handler} | stack]}
  end

  @doc """
  Extends the service to map the current reply to a new result.

  If an exception is raised before this point in the stack, maps the exception to a new
  result instead. However if this map, or a function added later in the stack raises
  it won't be rescued.
  """
  @spec map(t(req, rep), (rep -> res), (Exception.t(), Exception.stacktrace() -> res)) ::
          t(req, res)
        when req: var, rep: var, res: var
  def map(%Service{stack: stack} = s, mapper, handler)
      when is_function(mapper, 1) and is_function(handler, 2) do
    %Service{s | stack: [{:map_stacktrace, mapper, handler} | stack]}
  end

  @doc """
  Handle an exception raised before this point in the stack.
  """
  @spec handle(t(req, rep), (Exception.t() -> rep)) :: t(req, rep) when req: var, rep: var
  def handle(%Service{stack: stack} = s, handler) when is_function(handler, 1) do
    %Service{s | stack: [{:handle, handler} | stack]}
  end

  @doc """
  Handle an exception raised before this point in the stack.
  """
  @spec handle(t(req, rep), (Exception.t(), Exception.stacktrace() -> rep)) :: t(req, rep)
        when req: var, rep: var
  def handle(%Service{stack: stack} = s, handler) when is_function(handler, 2) do
    %Service{s | stack: [{:handle_stacktrace, handler} | stack]}
  end

  @doc """
  Extends the service to run a fun on the current reply, ignoring the result.
  """
  @spec each(t(req, rep), (req -> any)) :: t(req, rep) when req: var, rep: var
  def each(%Service{stack: stack} = s, runner) when is_function(runner, 1) do
    %Service{s | stack: [{:each, runner} | stack]}
  end

  @doc """
  Append two services so that output of first is input to second.
  """
  @spec append(t(req, rep), t(rep, res)) :: t(req, res) when req: var, rep: var, res: var
  def append(%Service{stack: stack1} = s, %Service{stack: stack2}) do
    %Service{s | stack: stack2 ++ stack1}
  end

  @doc """
  Append two services where output of first is mapped to input of second.
  """
  @spec append(t(req, rep), t(res1, res2), (rep -> res1)) :: t(req, res2)
        when req: var, rep: var, res1: var, res2: var
  def append(%Service{stack: stack1} = s, %Service{stack: stack2}, mapper)
      when is_function(mapper, 1) do
    %Service{s | stack: stack2 ++ [{:map, mapper} | stack1]}
  end

  @doc """
  Ensure a function is always run at the current point in the stack.
  """
  @spec ensure(t(req, rep), (() -> any())) :: t(req, rep) when req: var, rep: var
  def ensure(%Service{stack: stack} = s, ensurer) when is_function(ensurer, 0) do
    %Service{s | stack: [{:ensure, ensurer} | stack]}
  end

  @doc """
  Transform the service input and output.
  """
  @spec into(t(req, rep), (req2, (req -> rep) -> rep2)) :: t(req2, rep2)
        when req: var, rep: var, req2: var, rep2: var
  def into(%Service{stack: stack} = s, transformer) when is_function(transformer, 2) do
    %Service{s | stack: [{:into, transformer} | stack]}
  end

  @doc """
  Create an anonymous function that transforms the input to ouput.
  """
  @spec init(t(req, rep)) :: (req -> rep) when req: var, rep: var
  def init(%Service{stack: stack}) do
    &eval(stack, &1)
  end

  defp eval([], req), do: req
  defp eval([{:into, transformer} | stack], req), do: transformer.(req, &eval(stack, &1))
  defp eval([{:map, mapper} | stack], req), do: mapper.(eval(stack, req))

  defp eval([{:map, mapper, handler} | stack], req) do
    eval(stack, req)
  rescue
    error ->
      handler.(error)
  else
    res ->
      mapper.(res)
  end

  defp eval([{:map_stacktrace, mapper, handler} | stack], req) do
    eval(stack, req)
  catch
    :error, err ->
      stack = System.stacktrace()
      error = Exception.normalize(:error, err, stack)
      handler.(error, stack)
  else
    res ->
      mapper.(res)
  end

  defp eval([{:handle, handler} | stack], req) do
    eval(stack, req)
  rescue
    error ->
      handler.(error)
  end

  defp eval([{:handle_stacktrace, handler} | stack], req) do
    eval(stack, req)
  catch
    :error, err ->
      stack = System.stacktrace()
      error = Exception.normalize(:error, err, stack)
      handler.(error, stack)
  end

  defp eval([{:each, runner} | stack], req) do
    resp = eval(stack, req)
    _ = runner.(resp)
    resp
  end

  defp eval([{:ensure, ensurer} | stack], req) do
    eval(stack, req)
  after
    _ = ensurer.()
  end
end
