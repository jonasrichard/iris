defmodule Iris.Test.Unit.ChannelCreation do
  use ExUnit.Case

  test "after create a channel members get the first message" do
    events =
      Iris.Aggregate.Channel.create_channel(
        "111",
        "Channel name",
        "u1",
        ["u1", "u2", "u3"],
        "First message",
        Iris.Util.now_to_utc()
      )

    assert [_create_channel, send_first_message] = events
    assert "u1" in send_first_message.members
    assert "u2" in send_first_message.members
    assert "u3" in send_first_message.members
    assert "u1" == send_first_message.sender
    assert "First message" == send_first_message.body
    assert "111" == send_first_message.channel
  end

  test "create channel has a new channel event" do
    events =
      Iris.Aggregate.Channel.create_channel(
        "222",
        "Other channel",
        "u3",
        ["u1", "u3"],
        "Welcome",
        Iris.Util.now_to_utc()
      )

    [create_channel, _] = events

    assert "u3" == create_channel.owner
    assert "u1" in create_channel.members
    assert "u3" in create_channel.members
    assert "222" == create_channel.channel
  end
end
