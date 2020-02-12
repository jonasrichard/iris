defmodule Iris.Test.Unit.Projection.ChannelCreation do
  use ExUnit.Case

  test "after create channel users inbox contain the channel" do
    event = Iris.Fixture.Event.channel_created_event()

    Iris.Projection.Inbox.apply(event)

    owner_inbox = Iris.Database.Inbox.get_inbox(event.owner, event.channel)
    assert owner_inbox != nil

    assert owner_inbox.user_id == event.owner
    assert owner_inbox.channel_id == event.channel

    a_member = List.last(event.members)
    inbox = Iris.Database.Inbox.get_inbox(a_member, event.channel)
    assert inbox != nil

    assert inbox.user_id == a_member
    assert inbox.channel_id == event.channel
  end
end
