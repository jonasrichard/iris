defmodule Iris.MessageTest do
  use ExUnit.Case

  import Iris.Fixture, only: [retry: 1]

  alias Iris.Command.CreateChannel
  alias Iris.Command.SendMessage

  test "send message to an existing channel" do
    cmd1 = CreateChannel.new("About things", "user_1", ["user_1", "user_2"], "Hey, how are you?")
    cmd2 = SendMessage.new("user_2", cmd1.id, "Why do you ask?")

    [cmd1, cmd2] |> Iris.CommandDispatcher.send()

    inbox1 = retry(fn -> Iris.Database.Inbox.get_inbox("user_1", cmd1.id) end)
    assert inbox1.user_id == "user_1"
    assert inbox1.channel_id == cmd1.id
    assert inbox1.last_message == "Why do you ask?"

    inbox2 = retry(fn -> Iris.Database.Inbox.get_inbox("user_2", cmd1.id) end)
    assert inbox2.user_id == "user_2"
    assert inbox2.channel_id == cmd1.id
    assert inbox2.last_message == "Why do you ask?"
  end
end
