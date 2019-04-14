defmodule Iris.Projection.Inbox do
  def apply(%Iris.Event.InboxMessageArrived{} = event) do
    Iris.Database.Inbox.find_item!(event.user_id, event.channel_id)
    |> Map.put(:last_message, event.body)
    |> Map.put(:last_ts, event.message_ts)
    |> Iris.Database.Inbox.write!()
    nil
  end

  def apply(%Iris.Event.ChannelCreated{} = event) do
    %Iris.Database.Inbox{
      user_id: event.sender_id,
      channel_id: event.channel_id,
      last_message: nil,
      last_ts: nil
    }
    |> Iris.Database.Inbox.write!()
    nil
  end
end
