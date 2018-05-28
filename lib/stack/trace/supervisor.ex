defmodule Stack.Trace.Supervisor do
  @moduledoc false
  alias Stack.Trace
  alias Trace.{SpanProducer, SpanCache}
  use Supervisor

  @spec start_link() :: Supervisor.on_start()
  def start_link() do
    Supervisor.start_link(Trace.Supervisor, nil, name: Trace.Supervisor)
  end

  @impl Supervisor
  def init(_) do
    Supervisor.init([SpanProducer, SpanCache], strategy: :rest_for_one)
  end
end
