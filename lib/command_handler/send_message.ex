defmodule Iris.CommandHandler.SendMessage do
  def handle(command) do
    Iris.Aggregate.Channel.load(command.channel)
    |> Iris.Aggregate.Channel.send_message(
      command.id,
      command.sender,
      command.body,
      command.ts
    )
  end
end
