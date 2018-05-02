defmodule Stack.Trace.Probability do
  alias Stack.Trace.{Probability, Sampler}

  @default_probability 0.0001

  @enforce_keys [:p]
  defstruct [:p]

  @type t :: %Probability{p: float()}

  def new(p \\ @default_probability) do
    %Probability{p: p}
  end

  defimpl Sampler do
    def sample?(%Probability{p: p}) do
      :rand.uniform() < p
    end
  end
end
