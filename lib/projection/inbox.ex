defmodule Iris.Projection.Inbox do
  require Logger

  # TODO implement unread message number

  def apply(%Iris.Event.MessageSent{} = event) do
    Logger.info("Projecting #{inspect(event)}")
    # TODO this is incorrect, from projection we shouldn't read the current aggregate
    channel = Iris.Aggregate.Channel.load(event.channel)

    Logger.warn("Channel is #{inspect channel}")

    case channel do
      nil ->
        Logger.error("No aggregate for #{event.channel}")

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

  def get_user_inbox(user_id) do
    Iris.Database.Inbox.find_by_user_id(user_id)
  end

  defp create_inbox(user, channel) do
    Iris.Database.Inbox.write!(user, channel)
  end

  defp save_message_to_inbox(user, channel, sender, body, ts) do
    Iris.Database.Inbox.write!(user, channel, sender, body, ts)
  end
end
