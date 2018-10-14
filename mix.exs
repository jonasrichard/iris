defmodule Iris.Mixfile do
  use Mix.Project

  def project do
    [app: :iris,
     version: "0.1.0",
     elixir: "~> 1.3",
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
    [#{:cowboy, github: "ninenines/cowboy", tag: "2.0.0-pre.7"},
     {:credo, "~> 0.7", only: [:test, :dev]},
     #{:dogma, "~> 0.1.8", only: :dev},
     {:excoveralls, "~> 0.7.1", only: :test},
     {:httpoison, "~> 1.3.1", only: :test},
     {:instream, "~> 0.18"},
     {:poison, "~> 3.0"}
    ]
  end
end
