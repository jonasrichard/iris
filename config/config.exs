use Mix.Config

config :iris, Iris.Metrics,
  database: "iris",
  host: "localhost",
  port: 8086,
  scheme: "http",
  pool: [max_overflow: 10, size: 5]
  
