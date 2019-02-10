defmodule Iris.Mixfile do
  use Mix.Project

  def project do
    [app: :iris,
     version: "0.2.0",
     elixir: "~> 1.7",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_coverage: [tool: ExCoveralls],
     #preferred_cli_env: ["coveralls.html": :test],
     deps: deps()]
  end

  def application do
    [extra_applications: [:logger],
     mod: {Iris.App, []}]
  end

  defp deps do
    [{:credo, "~> 0.7", only: [:test, :dev]},
     {:distillery, "~> 2.0"},
     {:excoveralls, "~> 0.7.1", only: :test},
     {:httpoison, "~> 1.5.0", only: :test},
     {:instream, "~> 0.18"},
     {:jason, "~> 1.1"}
    ]
  end
end
