defmodule Stack.Application do
  @moduledoc false
  use Application
  alias Stack.{Cache, Trace}

  def start(_, _) do
    Supervisor.start_link(
      [Cache, Trace.Supervisor],
      strategy: :one_for_one,
      max_restart: 0,
      name: __MODULE__
    )
  end
end
