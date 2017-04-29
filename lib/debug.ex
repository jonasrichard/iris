defmodule Iris.Debug do
  def channels do
    Database.Channel.match!([]) |> IO.inspect
  end
end
