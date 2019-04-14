defmodule Iris.CommandHandler.SendMessage do
  def handle(command) do
    event = %Iris.Event.MessageSent{
      id: command.id,
      channel_id: command.channel_id,
      sender_id: command.sender_id,
      body: command.body,
      created_ts: command.created_ts
    }

    Iris.EventStore.append_event(:channel, command.channel_id, event)
  end
end
