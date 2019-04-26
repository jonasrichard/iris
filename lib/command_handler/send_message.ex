defmodule Iris.CommandHandler.SendMessage do
  def handle(command) do
    Iris.Aggregate.Channel.load(command.channel)
    |> Iris.Aggregate.Channel.send_message(
      command.id,
      UUID.uuid4(),    # generating message_id, is it good?
      command.sender,
      command.body,
      command.ts
    )
  end
end
