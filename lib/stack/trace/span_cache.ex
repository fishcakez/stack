defmodule Stack.Trace.SpanCache do
  @moduledoc false
  alias Stack.Deadline
  alias Stack.Trace.{SpanCache, SpanContext, SpanData}

  use GenServer, start: {SpanCache, :start_link, []}
  require Logger

  @spec start_link() :: GenServer.on_start()
  def start_link() do
    GenServer.start_link(SpanCache, nil, name: SpanCache)
  end

  @impl GenServer
  def init(_) do
    {:ok, 0}
  end

  @impl GenServer
  def handle_info({:"ETS-TRANSFER", data, pid, {SpanData, deadline}}, counter) do
    case Application.fetch_env!(:stack, :span_cache_size) do
      max_size when max_size > counter ->
        _ = Deadline.start_timer(deadline, self(), {SpanData, data})
        {:noreply, counter + 1}

      _ ->
        Logger.warn(fn ->
          "#{inspect(SpanCache)} dropped #{inspect(SpanContext)} from #{inspect(pid)}"
        end)

        {:noreply, counter}
    end
  end

  def handle_info({:timeout, _, {SpanData, data}}, counter) do
    SpanData.delete(data)
    {:noreply, counter - 1}
  end
end
