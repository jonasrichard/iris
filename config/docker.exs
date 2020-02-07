use Mix.Config

config :iris, :database,
  host: "cassandra:9042"

config :kafka_ex,
  brokers: [
    {"kafka", 9092}
  ]
