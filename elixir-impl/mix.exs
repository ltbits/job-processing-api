defmodule JobProcessor.MixProject do
  use Mix.Project

  def project do
    [
      app: :job_processor,
      version: "0.1.0",
      elixir: "~> 1.16.3",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {JobProcessor.Application, []}
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.7.1"},
      {:jason, "~> 1.4.0"},
      {:libgraph, "~> 0.16.0"}
    ]
  end
end
