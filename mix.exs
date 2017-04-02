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
    [extra_applications: [:logger, :cowboy]]
  end

  defp deps do
    [{:excoveralls, "~> 0.6", only: :test},
     {:cowboy, github: "ninenines/cowboy", tag: "2.0.0-pre.7"},
     #{:cowlib, "~> 1.0.2", override: true},
     {:dogma, "~> 0.1.8", only: :dev},
     {:excoveralls, "~> 0.6", only: :test},
     {:gun, github: "ninenines/gun"},
     {:httpoison, "~> 0.11.1", only: :test},
     {:poison, "~> 1.5.2"},
     {:ranch, "~> 1.3.2", override: true}
    ]
  end
end
