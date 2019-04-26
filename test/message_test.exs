defmodule Iris.MessageTest do
  use ExUnit.Case

  alias Iris.Command.CreateChannel
  alias Iris.Command.SendMessage

  test "send message to an existing channel" do
    cmd1 = CreateChannel.new("About things", "user_1", ["user_1", "user_2"], "Hey, how are you?")
    cmd2 = SendMessage.new("user_2", cmd1.id, "Why do you ask?")

    [cmd1, cmd2] |> Iris.CommandDispatcher.send()
    # TODO how to remove the sleep here?
    Process.sleep(100)

    inbox1 = Iris.Database.Inbox.find_item!("user_1", cmd1.id)
    assert inbox1.user_channel_id == {"user_1", cmd1.id}
    assert inbox1.last_message == "Why do you ask?"

    inbox2 = Iris.Database.Inbox.find_item!("user_2", cmd1.id)
    assert inbox2.user_channel_id == {"user_2", cmd1.id}
    assert inbox2.last_message == "Why do you ask?"
  end
end
