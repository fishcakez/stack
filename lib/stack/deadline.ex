defmodule Stack.Deadline do
  alias Stack.{Context, Deadline}

  @time_unit :nanosecond

  @enforce_keys [:start, :finish, :time_offset]
  defstruct [:start, :finish, :time_offset]

  @type t :: %Deadline{start: integer, finish: integer, time_offset: integer}

  @spec new(non_neg_integer) :: t
  def new(timeout) do
    start = System.monotonic_time(@time_unit)
    time_offset = System.time_offset(@time_unit)
    %Deadline{start: start, finish: start + timeout, time_offset: time_offset}
  end

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

  @spec bind(t | non_neg_integer, (() -> result)) :: result when result: var
  def bind(deadline, fun, args \\ [])

  @spec bind(t | non_neg_integer, (... -> result), list) :: result when result: var
  def bind(%Deadline{} = deadline, fun, args) do
    case Context.fetch(Deadline) do
      {:ok, old} ->
        Context.bind(Deadline, merge(old, deadline), fun, args)

      :error ->
        Context.bind(Deadline, deadline, fun, args)
    end
  end

  def bind(timeout, fun, args) do
    timeout
    |> new()
    |> bind(fun, args)
  end

  @spec start_timer(pid | atom, any) :: reference()
  @spec start_timer(t, pid | atom, any) :: reference()
  def start_timer(deadline \\ Context.fetch!(Deadline), dest, msg)

  def start_timer(%Deadline{finish: finish}, dest, msg) do
    abs = System.convert_time_unit(finish, @time_unit, :millisecond)
    :erlang.start_timer(abs, dest, msg, abs: true)
  end

  @spec timeout() :: non_neg_integer
  @spec timeout(t) :: non_neg_integer
  def timeout(deadline \\ Context.fetch!(Deadline))

  def timeout(%Deadline{finish: finish}) do
    now = System.monotonic_time(@time_unit)

    finish
    |> Kernel.-(now)
    |> System.convert_time_unit(@time_unit, :millisecond)
    |> max(0)
  end

  @spec filter(non_neg_integer) :: (req, (req -> rep) -> rep) when req: var, rep: var
  def filter(timeout), do: &bind(timeout, &2, [&1])
end
