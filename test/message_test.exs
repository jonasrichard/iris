defmodule Iris.MessageTest do
  use ExUnit.Case

  test "send message to an existing channel" do
    [
      %Iris.Command.CreateChannel{id: "2", name: "About things", sender_id: "user_1"},
      %Iris.Command.SendMessage{
        id: "3",
        sender_id: "user_1",
        channel_id: "2",
        body: "Hey, how are you?",
        created_ts: Iris.Util.now_to_utc()
      }
    ]
    |> Iris.CommandDispatcher.send()
    # TODO how to remove the sleep here?
    Process.sleep(100)

    inbox = Iris.Database.Inbox.find_item!("user_1", "2")
    assert inbox.user_channel_id == {"user_1", "2"}
    assert inbox.last_message == "Hey, how are you?"
  end
end
