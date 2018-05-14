defmodule Stack.Drop do
  @moduledoc """
  Drop exception.
  """
  alias Stack.Drop

  @type t :: %Drop{process: GenServer.name()}

  @enforce_keys [:process]
  defexception [:process]

  @impl Exception
  def exception(opts), do: %Drop{process: Keyword.get(opts, :process)}

  @impl Exception
  def message(%Drop{process: nil}), do: "dropped"
  def message(%Drop{process: name}), do: "dropped by #{inspect(name)}"
end
