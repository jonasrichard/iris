defmodule Iris.CommandHandler.SendMessage do
  require Logger

  def handle(command) do
    channel = Iris.Aggregate.Channel.load(command.channel)

    Logger.warn("#{inspect(channel)}")

    channel
    |> Iris.Aggregate.Channel.send_message(
      command.id,
      # generating message_id, is it good?
      UUID.uuid4(),
      command.sender,
      command.body,
      command.ts
    )
  end
end
