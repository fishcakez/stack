defmodule Stack.Trace.SpanData do
  @moduledoc false
  alias Stack.Trace.{SpanData, SpanProducer}

  @type t :: :ets.tid()

  @spec new([Trace.span_option()]) :: t()
  def new(span_opts) do
    :ets.new(SpanData, [:duplicate_bag | ets_opts(span_opts)])
  end

  @spec put(t, [{key, value}]) :: boolean when key: term, value: term
  def put(t, list) do
    try do
      :ets.insert(t, list)
    rescue
      ArgumentError ->
        false
    end
  end

  @spec put(t, key, value) :: boolean when key: term, value: term
  def put(data, key, value) do
    try do
      :ets.insert(data, {key, value})
    rescue
      ArgumentError ->
        false
    end
  end

  @spec delete(t()) :: boolean
  def delete(data) do
    case GenServer.whereis(SpanProducer) do
      producer when is_pid(producer) ->
        try do
          :ets.give_away(data, producer, SpanData)
        rescue
          ArgumentError ->
            :ets.delete(data)
            false
        end

      nil ->
        :ets.delete(data)
        false
    end
  end

  defp ets_opts(span_opts) do
    table_type = Keyword.get(span_opts, :span_data, :public)

    case Keyword.fetch(span_opts, :span_deadline) do
      {:ok, deadline} ->
        [table_type | heir_opts(SpanCache, {SpanData, deadline})]

      :error ->
        [table_type | heir_opts(SpanProducer, SpanData)]
    end
  end

  defp heir_opts(process, info) do
    case Process.whereis(process) do
      pid when is_pid(pid) ->
        [{:heir, pid, info}]

      nil ->
        []
    end
  end
end
