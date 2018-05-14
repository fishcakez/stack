defmodule Stack.CriticalityFilter do
  @moduledoc """
  Criticality handling with Stack.Filter.

  This module provides criticality handling via Stack.Filter.
  """
  alias Stack.{Filter, Criticality, CriticalityFilter}
  @behaviour Filter
   
  @doc """
  Create a filter that binds a criticality level inside the scope of the filter.

  ## Examples

      Stack.CriticalityFilter.new(:scheddable_plus)
      |> Filter.into(fn -> GenServer.call(server, {:request, Stack.Criticality.get()}) end)
  """
  @spec new(Criticality.t) :: Filter.t(req, rep, req, rep) when req: var, rep: var
  def new(level) when level in [:critical_plus, :critical, :scheddable_plus, :sheddable] do
    Filter.new(CriticalityFilter, level)
  end

  @doc false
  @impl Filter
  def init(level), do: level

  @doc false
  @impl Filter
  def call(req, service, level) do
    Criticality.bind(level, fn -> service.(req) end)
  end
end