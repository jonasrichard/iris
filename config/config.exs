use Mix.Config

config :iris, Iris.Metrics,
  database: "iris",
  host: "localhost",
  port: 8086,
  scheme: "http",
  pool: [max_overflow: 10, size: 5]

config :logger,
  level: :info,
  backends: [:console]

  #config :sasl,
  #sasl_error_logger: false

