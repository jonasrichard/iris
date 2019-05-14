defmodule Iris.Test.Acceptance.ChannelCreation do
  use ExUnit.Case

  test "channel creation with 3 members" do
    create_channel =
      Iris.Command.CreateChannel.new("Three amigos", "u1", ["u1", "u2", "u3"], "Hey guys")

    # send_message = Iris.Command.SendMessage.new("u2", create_channel.id, "Hey, whats up?")

    create_channel |> Iris.CommandDispatcher.send()

    # check both inboxes
    channel_id = create_channel.id
    u1_inbox = Iris.Database.Inbox.find_item!("u1", channel_id)

    assert u1_inbox.last_user_id == "u1"
    assert u1_inbox.last_message == "Hey guys"
    assert u1_inbox.last_ts == create_channel.ts

    u2_inbox = Iris.Database.Inbox.find_item!("u2", channel_id)

    assert u2_inbox.last_user_id == "u1"
    assert u2_inbox.last_message == "Hey guys"
    assert u2_inbox.last_ts == create_channel.ts
  end
end
