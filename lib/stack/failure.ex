defmodule Stack.Failure do
  @moduledoc """
  Failure exception.
  """
  alias Stack.{Failure, Criticality}

  @type type :: :reject | :retry | :error
  @type t :: %Failure{type: type, value: term, criticality: Criticality.t()}

  @enforce_keys [:type, :value, :criticality]
  defexception [:type, :value, :criticality]

  @impl Exception
  def exception(opts) do
    %Failure{
      type: Keyword.get(opts, :type, :error),
      value: Keyword.get(opts, :value),
      criticality: Keyword.get(opts, :criticality, :critical)
    }
  end

  @impl Exception
  def message(%Failure{type: type, value: nil, criticality: level}),
    do: "#{level} #{type} failure"

  def message(%Failure{type: type, value: value, criticality: level}) do
    if Exception.exception?(value) do
      "** " <> message = Exception.format_banner(:error, value, [])
      "#{level} #{type} failure with #{message}"
    else
      "#{level} #{type} failure with #{inspect(value)}"
    end
  end
end
