use Mix.Config

config :iris, :database, host: "localhost:9042"

config :kafka_ex,
  brokers: [
    {"localhost", 9092}
  ]
