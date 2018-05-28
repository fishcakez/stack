defmodule Stack.Trace.SpanProducer do
  @moduledoc false
  alias Stack.Trace.{SpanProducer, SpanData, SpanContext, Span}
  use GenStage, start: {SpanProducer, :start_link, []}
  @behaviour GenStage
  require Logger

  @spec start_link() :: GenServer.on_start()
  def start_link() do
    GenStage.start_link(SpanProducer, nil, name: SpanProducer)
  end

  @impl GenStage
  def init(_) do
    {:producer, 0}
  end

  @impl GenStage
  def handle_demand(demand, pending) do
    {:noreply, demand + pending}
  end

  @impl GenStage
  def handle_info({:"ETS-TRANSFER", data, pid, SpanData}, pending) do
    cond do
      pending == 0 ->
        Logger.warn(fn ->
          "#{inspect(SpanProducer)} dropped #{inspect(SpanContext)} from #{inspect(pid)}"
        end)

        {:noreply, [], pending}

      :ets.info(data, :size) == 0 ->
        # Handle race condition where heir setup but initial data not added!
        {:noreply, [], pending}

      true ->
        span_info = :ets.tab2list(data)
        :ets.delete(data)
        {:noreply, [Span.new(span_info)], pending - 1}
    end
  end
end
