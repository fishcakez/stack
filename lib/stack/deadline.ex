defmodule Stack.Deadline do
  @moduledoc """
  Deadline handling.

  This modules provides deadlines over (multiple) Stack.Contexts. The nearest deadline in
  the current scope takes priority when using a context.
  """
  alias Stack.{Context, Deadline}

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
    start = System.monotonic_time()
    finish = start + System.convert_time_unit(timeout, time_unit, :native)
    time_offset = System.time_offset()
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
    abs = System.convert_time_unit(finish, :native, :millisecond)
    :erlang.start_timer(abs, dest, msg, abs: true)
  end

  @doc """
  Get the remaining time from the deadline in the current scope.

  If a deadline is not bound in the current scope returns `:infinity`.
  """
  @spec timeout() :: timeout
  def timeout() do
    case Context.fetch(Deadline) do
      {:ok, deadline} ->
        timeout(deadline)

      :error ->
        :infinity
    end
  end

  @doc """
  Get the remaining time from a deadline.
  """
  @spec timeout(t) :: non_neg_integer
  def timeout(%Deadline{finish: finish}) do
    now = System.monotonic_time()

    finish
    |> Kernel.-(now)
    |> System.convert_time_unit(:native, :millisecond)
    |> max(0)
  end

  @doc """
  Bind a deadline to the scope of an anonymouns function and run the function.

  The first argument is a deadline struct. If a deadline already exists in the
  current context the deadlines are merged.

  ## Examples
      Stack.Deadline.bind(Stack.Deadline.new(1000), fn ->
        GenServer.call(MyServer, :request, Stack.Deadline.timeout())
      end)
  """
  @spec bind(t, (() -> result)) :: result when result: var
  def bind(%Deadline{} = deadline, fun) do
    case Context.fetch(Deadline) do
      {:ok, old} ->
        Context.bind(Deadline, merge(old, deadline), fun)

      :error ->
        Context.bind(Deadline, deadline, fun)
    end
  end
end
