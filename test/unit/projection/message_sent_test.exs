defmodule Iris.Test.Unit.Projection.MessageSent do
  use ExUnit.Case

  test "message sent should update the inboxes" do
    event1 = Iris.Fixture.Event.channel_created_event()
    sender = List.last(event1.members)
    event2 = Iris.Fixture.Event.message_sent_event(sender: sender, channel: event1.channel, members: event1.members)

    [event1, event2]
    |> Enum.each(&Iris.Projection.Inbox.apply/1)

    inbox1 = Iris.Database.Inbox.get_inbox(event1.owner, event1.channel)
    assert inbox1.last_user_id == event2.sender
    assert inbox1.last_message == event2.body
    equal(inbox1.last_ts, event2.ts)
  end

  def equal(utc_dt, iso_dt) do
    n1 = DateTime.to_naive(utc_dt)
    {:ok, n2} = NaiveDateTime.from_iso8601(iso_dt)
    assert NaiveDateTime.compare(n1, n2) == :eq
  end
end
