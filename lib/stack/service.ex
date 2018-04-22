defmodule Stack.Service do
  alias Stack.Service

  defstruct stack: []

  @opaque t(_req, _rep) :: %Service{}

  @spec new() :: t(req, req) when req: var
  def new(), do: %Service{}

  @spec map(t(req, rep), (rep -> res)) :: t(req, res) when req: var, rep: var, res: var
  def map(%Service{stack: stack} = s, mapper) when is_function(mapper, 1) do
    %Service{s | stack: [{:map, mapper} | stack]}
  end

  @spec map(t(req, rep), (rep -> res), (Exception.t() -> res)) :: t(req, res)
        when req: var, rep: var, res: var
  def map(%Service{stack: stack} = s, mapper, handler)
      when is_function(mapper, 1) and is_function(handler, 1) do
    %Service{s | stack: [{:map, mapper, handler} | stack]}
  end

  @spec map(t(req, rep), (rep -> res), (Exception.t(), Exception.stacktrace() -> res)) ::
          t(req, res)
        when req: var, rep: var, res: var
  def map(%Service{stack: stack} = s, mapper, handler)
      when is_function(mapper, 1) and is_function(handler, 2) do
    %Service{s | stack: [{:map_stacktrace, mapper, handler} | stack]}
  end

  @spec handle(t(req, rep), (Exception.t() -> rep)) :: t(req, rep) when req: var, rep: var
  def handle(%Service{stack: stack} = s, handler) when is_function(handler, 1) do
    %Service{s | stack: [{:handle, handler} | stack]}
  end

  @spec handle(t(req, rep), (Exception.t(), Exception.stacktrace() -> rep)) :: t(req, rep)
        when req: var, rep: var
  def handle(%Service{stack: stack} = s, handler) when is_function(handler, 2) do
    %Service{s | stack: [{:handle_stacktrace, handler} | stack]}
  end

  @spec each(t(req, rep), (req -> any)) :: t(req, rep) when req: var, rep: var
  def each(%Service{stack: stack} = s, runner) when is_function(runner, 1) do
    %Service{s | stack: [{:each, runner} | stack]}
  end

  @spec merge(t(req, rep), t(rep, res)) :: t(req, res) when req: var, rep: var, res: var
  def merge(%Service{stack: stack1} = s, %Service{stack: stack2}) do
    %Service{s | stack: stack2 ++ stack1}
  end

  @spec merge(t(req, rep), t(res1, res2), (rep -> res1)) :: t(req, res2)
        when req: var, rep: var, res1: var, res2: var
  def merge(%Service{stack: stack1} = s, %Service{stack: stack2}, mapper)
      when is_function(mapper, 1) do
    %Service{s | stack: stack2 ++ [{:map, mapper} | stack1]}
  end

  @spec ensure(t(req, rep), (() -> any())) :: t(req, rep) when req: var, rep: var
  def ensure(%Service{stack: stack} = s, ensurer) when is_function(ensurer, 0) do
    %Service{s | stack: [{:ensure, ensurer} | stack]}
  end

  @spec wrap(t(req, rep), (req2, (req -> rep) -> rep2)) :: t(req2, rep2)
        when req: var, rep: var, req2: var, rep2: var
  def wrap(%Service{stack: stack} = s, transformer) when is_function(transformer, 2) do
    %Service{s | stack: [{:into, transformer} | stack]}
  end

  @spec init(t(req, rep)) :: (req -> rep) when req: var, rep: var
  def init(%Service{stack: stack}) do
    &eval(stack, &1)
  end

  @spec call(t(req, rep), req) :: rep when req: var, rep: var
  def call(%Service{stack: stack}, req) do
    eval(stack, req)
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
