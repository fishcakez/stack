defmodule Stack.DeadlineFilter do
  @moduledoc """
  Deadline handling with Stack.Filter.

  This module provides deadline handling via Stack.Filter.
  """
  alias Stack.{Filter, Deadline, DeadlineFilter}
  @behaviour Filter

  @doc """
  Create a filter that binds a deadline inside the scope of the filter.

  ## Examples

      Stack.DeadlineFilter.new(100)
      |> Filter.into(fn -> GenServer.call(server, :request, Stack.Deadline.timeout()) end)
  """
  @spec new(non_neg_integer) :: Filter.t(req, rep, req, rep) when req: var, rep: var
  def new(timeout) when is_integer(timeout) and timeout >= 0 do
    Filter.new(DeadlineFilter, timeout)
  end

  @doc false
  @impl Filter
  def init(timeout), do: timeout

  @doc false
  @impl Filter
  def call(req, service, timeout) do
    timeout
    |> Deadline.new()
    |> Deadline.bind(fn -> service.(req) end)
  end
end
