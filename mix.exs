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
    [extra_applications: [:logger], mod: {Stack.Application, []}, env: [span_cache_size: 1_000]]
  end

  defp deps do
    [
      {:gen_stage, "~> 0.13"},
      {:backoff, "~> 1.1.6 or ~> 1.2", optional: true},
      {:sbroker, "~> 1.0", optional: true}
    ]
  end
end
