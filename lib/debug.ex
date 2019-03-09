defmodule Iris.Debug do
  def channels do
    Iris.Database.Channel.match!([])
    |> IO.inspect
  end

  def events do
    Iris.Database.Event.match!([])
    |> IO.inspect
  end
end
