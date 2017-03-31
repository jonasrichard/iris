defmodule Iris.Mixfile do
  use Mix.Project

  def project do
    [app: :iris,
     version: "0.1.0",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [extra_applications: [:logger, :cowboy]]
  end

  defp deps do
    [{:coverex, "~> 1.4.12", only: :test},
     {:cowboy, "~> 1.1.2"},
     {:cowlib, "~> 1.0.2", override: true},
     {:dogma, "~> 0.1.8", only: :dev},
     {:gun, git: "git://github.com/ninenines/gun.git"},
     {:httpoison, "~> 0.11.1", only: :test},
     {:ranch, "~> 1.3.2", override: true},
     {:poison, "~> 1.5.2"}]
  end
end
