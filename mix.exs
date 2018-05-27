defmodule Stack.MixProject do
  use Mix.Project

  def project do
    [
      app: :stack,
      version: "0.1.0",
      elixir: "~> 1.6.5 or ~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    []
  end

  defp deps do
    [{:backoff, "~> 1.1.6 or ~> 1.2", optional: true},
     {:sbroker, "~> 1.0", optional: true}]
  end
end
