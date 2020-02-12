defmodule Iris.Test.Unit.Aggregate.ChannelCreation do
  use ExUnit.Case

  test "create channel are handled by the aggregate" do
    channel =
      Iris.Aggregate.Channel.create_channel(
        "111",
        "Channel name",
        "u1",
        ["u1", "u2", "u3"],
        "First message",
        Iris.Util.now_to_utc()
      )

    assert channel.id == "111"
    assert channel.name == "Channel name"
    assert channel.owner == "u1"
    assert channel.members == ["u1", "u2", "u3"]
  end
end
