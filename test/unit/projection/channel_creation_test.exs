defmodule Iris.Test.Unit.Projection.ChannelCreation do
  use ExUnit.Case

  test "after create channel users inbox contain the channel" do
    event = Iris.Fixture.Event.channel_created_event()

    Iris.Projection.Inbox.apply(event)

    owner_inbox = Iris.Database.Inbox.get_inbox(event.owner, event.channel)
    assert owner_inbox != nil
  end
end
