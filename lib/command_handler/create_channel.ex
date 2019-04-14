defmodule Iris.CommandHandler.CreateChannel do
  def handle(command) do
    event = %Iris.Event.ChannelCreated{
      id: "event_id",
      channel_id: command.id,
      name: command.name,
      sender_id: command.sender_id
    }

    Iris.EventStore.append_event(:channel, command.id, event)
  end
end
