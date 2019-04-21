defmodule Iris.CommandHandler.CreateChannel do
  alias Iris.Aggregate.Channel

  @doc ~S"""
  Handle the `ChannelCreated` command
  """
  @spec handle(%Iris.Command.CreateChannel{}) :: term()
  def handle(command) do
    case Channel.load(command.id) do
      nil ->
        Channel.create_channel(
          command.id,
          command.name,
          command.sender,
          command.members,
          command.first_message,
          command.ts
        )

      _ ->
        {:error, :already_exists}
    end
  end
end
