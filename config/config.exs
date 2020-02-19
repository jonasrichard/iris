use Mix.Config

config :iris, Iris.Metrics,
  database: "iris",
  host: "localhost",
  port: 8086,
  scheme: "http",
  pool: [max_overflow: 10, size: 5]

config :iris, database_type: :cassandra

config :logger,
  level: :info,
  backends: [:console, {LoggerFileBackend, :iris_log}],
  handle_sasl_reports: true

config :logger, :console, metadata: [:module, :function, :line]

config :logger, :iris_log,
  format: "\n$date $time $metadata[$level] $levelpad$message\n",
  path: "log/iris.log",
  level: :info

import_config "#{Mix.env()}.exs"
