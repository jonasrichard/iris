defmodule Iris.Projection.Inbox do
  require Logger

  def apply(%Iris.Event.MessageSent{} = event) do
    Logger.info("Projecting #{inspect event}")
    channel = Iris.Aggregate.Channel.load(event.channel)

    for user <- channel.members do
      save_message_to_inbox(user, event.channel, event.sender, event.body, event.ts)
    end
  end

  def apply(%Iris.Event.ChannelCreated{} = event) do
    for user <- event.members do
      create_inbox(user, event.channel)
    end
  end

  defp create_inbox(user, channel) do
    %Iris.Database.Inbox{
      user_channel_id: {user, channel}
    }
    |> Iris.Database.Inbox.write!()
  end

  defp save_message_to_inbox(user, channel, sender, body, ts) do
    %Iris.Database.Inbox{
      user_channel_id: {user, channel},
      last_user_id: sender,
      last_message: body,
      last_ts: ts
    }
    |> Iris.Database.Inbox.write!()
  end
end
