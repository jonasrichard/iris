defmodule Iris.Projection.Inbox do
  require Logger

  # TODO implement unread message number

  def apply(%Iris.Event.MessageSent{} = event) do
    Logger.info("Projecting #{inspect(event)}")
    channel = Iris.Aggregate.Channel.load(event.channel)

    case channel do
      nil ->
        Logger.error("No aggregate for #{event.channel}")

      # Logger.warning("Channels are #{inspect Iris.Debug.channels()}")

      _ ->
        for user <- channel.members do
          save_message_to_inbox(user, event.channel, event.sender, event.body, event.ts)
        end
    end
  end

  def apply(%Iris.Event.ChannelCreated{} = event) do
    for user <- event.members do
      create_inbox(user, event.channel)
    end
  end

  def apply(_) do
    :ok
  end

  defp create_inbox(user, channel) do
    %Iris.Mnesia.Inbox{
      user_channel_id: {user, channel}
    }
    |> Iris.Mnesia.Inbox.write!()
  end

  defp save_message_to_inbox(user, channel, sender, body, ts) do
    %Iris.Mnesia.Inbox{
      user_channel_id: {user, channel},
      last_user_id: sender,
      last_message: body,
      last_ts: ts
    }
    |> Iris.Mnesia.Inbox.write!()
  end
end
