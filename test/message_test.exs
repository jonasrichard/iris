defmodule Iris.MessageTest do
  use ExUnit.Case

  alias Iris.Command.CreateChannel
  alias Iris.Command.SendMessage

  test "send message to an existing channel" do
    cmd1 = CreateChannel.new("About things", "user_1", ["user_2"], "Hey, how are you?")
    cmd2 = SendMessage.new("user_2", cmd1.id, "Why do you ask?")

    [cmd1, cmd2] |> Iris.CommandDispatcher.send()
    # TODO how to remove the sleep here?
    Process.sleep(100)

    inbox = Iris.Database.Inbox.find_item!("user_1", cmd1.id)
    assert inbox.user_channel_id == {"user_1", cmd1.id}
    assert inbox.last_message == "Hey, how are you?"
  end
end
