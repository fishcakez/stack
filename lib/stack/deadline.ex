defmodule Stack.Deadline do
  @moduledoc """
  Deadline handling with Stack.Filter via Stack.Context.

  This modules provides deadlines over (multiple) Stack.Contexts. The nearest deadline in
  the current scope takes priority when using a context.
  """
  alias Stack.{Context, Deadline, Filter}

  @time_unit :nanosecond

  @enforce_keys [:start, :finish, :time_offset]
  defstruct [:start, :finish, :time_offset]

  @typedoc """
  Deadline struct.
  """
  @type t :: %Deadline{start: integer, finish: integer, time_offset: integer}

  @doc """
  Create a deadline from a timeout.

  The deadline is in `timeout` with time unit `time_unit`.

  ## Examples

      Stack.Deadline.new(100, :millisecond)
  """
  @spec new(non_neg_integer, System.time_unit()) :: t
  def new(timeout, time_unit \\ :millisecond) do
    start = System.monotonic_time(@time_unit)
    finish = start + System.convert_time_unit(timeout, time_unit, @time_unit)
    time_offset = System.time_offset(@time_unit)
    %Deadline{start: start, finish: finish, time_offset: time_offset}
  end

  @doc """
  Merge two deadlines to the nearest deadline.

  ## Examples

      Stack.Deadline.merge(Stack.Deadline.new(100), Stack.Deadline.new(10))
  """
  @spec merge(t, t) :: t
  def merge(deadline1, deadline2) do
    %Deadline{start: start1, finish: finish1} = deadline1
    %Deadline{start: start2, finish: finish2} = deadline2
    # most recent start/time_offset (accuracy) and earliest finish (strict)
    finish = min(finish1, finish2)

    if start1 > start2 do
      %Deadline{deadline1 | finish: finish}
    else
      %Deadline{deadline2 | finish: finish}
    end
  end

  @doc """
  Start a timer using the deadline in the current scope.

  If a deadline is not bound in the current scope raises `KeyError`.

  ## Examples

      Stack.Deadline.start_timer(self(), :stop)
  """
  @spec start_timer(pid | atom, any) :: reference()
  def start_timer(deadline \\ Context.fetch!(Deadline), dest, msg)

  @doc """
  Start a timer with a deadline.

  ## Examples

      Stack.Deadline.start_timer(Stack.Deadline.new(100), self(), :stop)
  """
  @spec start_timer(t, pid | atom, any) :: reference()
  def start_timer(%Deadline{finish: finish}, dest, msg) do
    abs = System.convert_time_unit(finish, @time_unit, :millisecond)
    :erlang.start_timer(abs, dest, msg, abs: true)
  end

  @doc """
  Get the remaining time from the deadline in the current scope.

  If a deadline is not bound in the current scope raises `KeyError`.
  """
  @spec timeout() :: non_neg_integer
  def timeout(deadline \\ Context.fetch!(Deadline))

  @doc """
  Get the remaining time from a deadline.
  """
  @spec timeout(t) :: non_neg_integer
  def timeout(%Deadline{finish: finish}) do
    now = System.monotonic_time(@time_unit)

    finish
    |> Kernel.-(now)
    |> System.convert_time_unit(@time_unit, :millisecond)
    |> max(0)
  end

  @doc """
  Bind a deadline to the scope of an anonymouns function and run the function.

  The first argument is either a deadline struct or a non-infinity timeout
  (creating a new deadline). If a deadline already exists in the current context
  the deadlines are merged.

  ## Examples
      Mux.Deadline.bind(1000, fn ->
        GenServer.call(MyServer, :request, Mux.Deadline.timeout())
      end)
  """
  @spec bind(t | non_neg_integer, (() -> result)) :: result when result: var
  def bind(%Deadline{} = deadline, fun) do
    case Context.fetch(Deadline) do
      {:ok, old} ->
        Context.bind(Deadline, merge(old, deadline), fun)

      :error ->
        Context.bind(Deadline, deadline, fun)
    end
  end

  def bind(timeout, fun) do
    timeout
    |> new()
    |> bind(fun)
  end

  @doc """
  Create a filter that binds a deadline inside the scope of the filter.

  ## Examples

      Stack.Deadline.filter(100)
      |> Filter.into(fn -> GenServer.call(server, :request, Stack.Deadline.timeout()) end)
  """
  @spec filter(non_neg_integer) :: Filter.t(req, rep, req, rep) when req: var, rep: var
  def filter(timeout) when is_integer(timeout) and timeout >= 0 do
    Filter.new(fn req, service -> bind(timeout, fn -> service.(req) end) end)
  end
end
