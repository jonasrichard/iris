defmodule Iris.Mixfile do
  use Mix.Project

  def project do
    [app: :iris,
     version: "0.1.0",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_coverage: [tool: ExCoveralls],
     deps: deps()]
  end

  def application do
    [extra_applications: [:logger, :cowboy, :mnesia],
     mod: {Iris.App, []}]
  end

  defp deps do
    [{:amnesia, "~> 0.2.7"},
     {:cowboy, "~> 2.3.0"},
     {:cowlib, "~> 2.2.1", override: true},
     {:credo, "~> 0.9.1", only: [:test, :dev]},
     #{:dogma, "~> 0.1.8", only: :dev},
     {:excoveralls, "~> 0.8.1", only: :test},
     {:gun, github: "ninenines/gun"},
     {:httpoison, "~> 1.1.0", only: :test},
     {:poison, "~> 3.1.0"},
     {:ranch, "~> 1.5.0", override: true}
    ]
  end
end
