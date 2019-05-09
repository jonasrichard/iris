defmodule Iris.Mixfile do
  use Mix.Project

  def project do
    [
      app: :iris,
      version: "0.2.0",
      elixir: "~> 1.7",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      # preferred_cli_env: ["coveralls.html": :test],
      deps: deps()
    ]
  end

  def application do
    [extra_applications:
      [
        :runtime_tools,
        #:sasl,
        :logger,
        :mnesia,
        :kafka_ex,
        :cowboy
      ],
     mod: {Iris.App, []}]
  end

  defp deps do
    [
      {:amnesia, github: "meh/amnesia"},
      {:cowboy, "~> 2.6"},
      {:credo, "~> 0.7", only: [:test, :dev]},
      {:distillery, "~> 2.0"},
      {:elixir_uuid, "~> 1.2"},
      {:excoveralls, "~> 0.10", only: :test},
      {:exquisite, github: "meh/exquisite", override: true},
      {:httpoison, "~> 1.5.0", only: :test},
      {:instream, "~> 0.18"},
      {:jason, "~> 1.1"},
      {:kafka_ex, "~> 0.9.0"},
      {:logger_file_backend, "~> 0.0"},
      {:plug, "~> 1.7"},
      {:plug_cowboy, "~> 2.0"},
      {:rexbug, ">= 1.0.0"}
    ]
  end
end
