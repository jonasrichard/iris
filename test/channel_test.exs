defmodule Iris.ChannelTest do
  use ExUnit.Case
  require Logger

  alias Iris.Command.CreateChannel

  test "create channel aggregate" do
    cmd = CreateChannel.new("name #1", "u1", ["u1", "u2"], "Hey!")

    Iris.CommandHandler.CreateChannel.handle(cmd)

    channel = Iris.Aggregate.Channel.load(cmd.id)
    assert channel.id == cmd.id
    assert channel.name == "name #1"
    assert channel.owner == "u1"
  end

  test "create channel" do
    cmd = Iris.Command.CreateChannel.new("new channel", "user_1", ["user_1"], "Zero")
    cmd |> Iris.CommandDispatcher.send()

    channel = Iris.Aggregate.Channel.load(cmd.id)
    assert channel.id == cmd.id
    assert channel.name == "new channel"
    assert channel.owner == "user_1"
  end

  # We need to move this test into projection-level test
  test "channel with 3 members get messages" do
    create_channel = Iris.Command.CreateChannel.new("Three amigos", "u1", ["u1", "u2", "u3"], "Hey guys")
    send_message = Iris.Command.SendMessage.new("u2", create_channel.id, "Hey, whats up?")

    [create_channel, send_message] |> Iris.CommandDispatcher.send()

  end
end
