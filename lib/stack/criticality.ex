defmodule Stack.Criticality do
  @moduledoc """
  Criticality handling.

  This modules provides criticality over (multiple) Stack.Contexts. The highest criticality in
  the current scope takes priority when using a context.
  """
  alias Stack.{Context, Criticality}

  @typedoc """
  The criticality levels.

  ## Levels

    * `:critical_plus` - the highest level for requests that have high impact on user experience
    * `:critical` - for requests that can directly impact user experience
    * `:scheddable_plus` - for requests that can be dropped or retried significantly later without
    impact on user experience
    * `:scheddable` - for requests that are speculative or can be dropped without ever retrying
  """
  @type t :: :critical_plus | :critical | :sheddable_plus | :sheddable

  @doc """
  Compare criticality levels.

  Receives two criticality levels and compares the left level against the right level and
  returns

  • `:lt` if `left` is less than `right`
  • `:eq` if `left` and `right` are equal
  • `:gt` if `left` is greater than `right`
  """
  @spec compare_levels(t(), t()) :: :lt | :eq | :gt
  def compare_levels(:critical_plus, :critical_plus), do: :eq
  def compare_levels(:critical_plus, _), do: :gt
  def compare_levels(:critical, :critical_plus), do: :lt
  def compare_levels(:critical, :critical), do: :eq
  def compare_levels(:critical, _), do: :gt
  def compare_levels(:sheddable_plus, :sheddable), do: :gt
  def compare_levels(:sheddable_plus, :sheddable_plus), do: :eq
  def compare_levels(:sheddable_plus, _), do: :lt
  def compare_levels(:sheddable, :sheddable), do: :eq
  def compare_levels(:sheddable, _), do: :lt

  @doc """
  Bind a criticality level to the scope of an anonymouns function and run the function.

  The first argument is a criticality level.

  ## Examples
      Stack.Criticality.bind(:sheddable, fn ->
        GenServer.call(MyServer, {:request, Stack.Criticality.get()})
      end)
  """
  @spec bind(t(), (() -> result)) :: result when result: var
  def bind(criticality, fun)
      when criticality in [:critical_plus, :critical, :sheddable_plus, :sheddable] do
    Context.bind(Criticality, criticality, fun)
  end

  @doc """
  Get the criticality level bound in the current scope.

  If a criticality level is not bound in the current scope returns `:critical`.
  """
  @spec level() :: t()
  def level(), do: Context.get(Criticality, :critical)
end
