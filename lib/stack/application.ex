defmodule Stack.Application do
  @moduledoc false
  use Application

  def start(_, _) do
    Stack.Trace.Supervisor.start_link()
  end
end
