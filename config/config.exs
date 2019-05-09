use Mix.Config

config :iris, Iris.Metrics,
  database: "iris",
  host: "localhost",
  port: 8086,
  scheme: "http",
  pool: [max_overflow: 10, size: 5]

config :logger,
  level: :info,
  backends: [:console, {LoggerFileBackend, :iris_log}],
  handle_sasl_reports: true

config :logger, :iris_log,
  path: "log/iris.log",
  level: :info

config :kafka_ex,
  brokers: [
    {"kafka", 9092}
  ]

