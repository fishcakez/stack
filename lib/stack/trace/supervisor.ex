defmodule Stack.Trace.Supervisor do
  @moduledoc false
  alias Stack.Trace
  alias Trace.{SpanProducer, SpanCache}
  use Supervisor

  @spec start_link([Supervisor.init_option()]) :: Supervisor.on_start()
  def start_link(opts) do
    Supervisor.start_link(Trace.Supervisor, opts, name: Trace.Supervisor)
  end

  @impl Supervisor
  def init(opts) do
    Supervisor.init([SpanProducer, SpanCache], [strategy: :rest_for_one] ++ opts)
  end
end
