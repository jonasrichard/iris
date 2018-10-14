defmodule Iris.Metrics.Erlang do
  use Instream.Series

  series do
    database      "iris"
    measurement   "erlang"

    tag :server

    field   :total
    field   :processes
  end
end
