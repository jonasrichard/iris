defmodule Iris.Projection.Inbox do
  require Logger

  def apply(%Iris.Event.MessageSent{} = event) do
    Logger.info("Projecting #{inspect event}")
    channel = Iris.Aggregate.Channel.load(event.channel)

    send_message(event.sender, event.channel, channel.members, event.body, event.ts)
  end

  def apply(%Iris.Event.ChannelCreated{} = event) do
    for user <- [event.owner | event.members] do
      create_inbox(user, event.channel)
    end
  end

  defp create_inbox(owner, channel) do
    %Iris.Database.Inbox{
      user_channel_id: {owner, channel}
    }
    |> Iris.Database.Inbox.write!()
  end

  defp send_message(sender, channel, members, body, ts) do
    send_message_to_inbox(sender, channel, sender, body, ts)
    for user <- members do
      send_message_to_inbox(user, channel, sender, body, ts)
    end
  end

  defp send_message_to_inbox(user, channel, sender, body, ts) do
    %Iris.Database.Inbox{
      user_channel_id: {user, channel},
      last_user_id: sender,
      last_message: body,
      last_ts: ts
    }
    |> Iris.Database.Inbox.write!()
  end
end
