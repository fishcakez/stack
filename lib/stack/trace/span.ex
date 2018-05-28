defmodule Stack.Trace.Span do
  alias Stack.Trace
  alias Trace.Span

  @enforce_keys [:trace_id, :parent_id, :span_id, :attributes]
  defstruct [:trace_id, :parent_id, :span_id, :attributes]

  @typedoc """
  Span information.
  """
  @type t :: %Span{
          trace_id: Trace.trace_id(),
          parent_id: Trace.span_id(),
          span_id: Trace.span_id(),
          attributes: [{term, term}]
        }

  @doc false
  @spec new([{term, term}, ...]) :: t()
  def new(info) do
    {{_, trace_id}, info} = List.keytake(info, 0, :trace_id)
    {{_, parent_id}, info} = List.keytake(info, 0, :parent_id)
    {{_, span_id}, info} = List.keytake(info, 0, :span_id)
    %Span{trace_id: trace_id, parent_id: parent_id, span_id: span_id, attributes: info}
  end
end
