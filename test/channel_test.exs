defmodule Iris.ChannelTest do
  use ExUnit.Case
  require Logger

  test "create channel" do
    %Iris.Command.CreateChannel{id: "1", name: "new channel", sender_id: "user_1"}
    |> Iris.CommandDispatcher.send()

    channel = Iris.Aggregate.Channel.load("1")
    assert channel.id == "1"
    assert channel.name == "new channel"
    assert channel.owner == "user_1"
  end
end
