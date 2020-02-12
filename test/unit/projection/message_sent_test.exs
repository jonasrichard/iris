defmodule Iris.Test.Unit.Projection.MessageSent do
  use ExUnit.Case

  test "message sent should update the inboxes" do
    event1 = Iris.Fixture.Event.channel_created_event()
    event2 = Iris.Fixture.Event.message_sent_event()

    [event1, event2] |> Iris.CommandDispatcher.send()

    Process.sleep(1000)

    inbox1 = Iris.Database.Inbox.get_inbox(event1.owner, event1.channel)
    assert inbox1.last_user_id == event2.sender
    assert inbox1.last_message == event2.body
    assert inbox1.last_ts == event2.ts
  end
end
