defmodule Iris.Projection.Inbox do
  require Logger

  def apply(%Iris.Event.InboxMessageArrived{} = event) do
    Logger.info("Projecting #{inspect event}")
    inbox = Iris.Database.Inbox.find_item!(event.user_id, event.channel_id)
    %{inbox |
      last_user_id: event.sender_id,
      last_message: event.body,
      last_ts: event.message_ts}
    |> Iris.Database.Inbox.write!()

    Iris.Debug.inbox

    nil
  end

  def apply(%Iris.Event.ChannelCreated{} = event) do
    %Iris.Database.Inbox{
      user_channel_id: {event.sender_id, event.channel_id},
      last_user_id: event.sender_id,
      last_message: nil,
      last_ts: nil
    }
    |> Iris.Database.Inbox.write!()
    nil
  end
end
