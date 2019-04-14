defmodule Iris.Channel.Processor do
  alias Iris.Database.Channel, as: Channel
  alias Iris.Database.Messages, as: Messages

  # @spec process(%Iris.Database.Event{}) :: term
  def process(event) do
    # case event.message.__struct__ do
    # Iris.Model.Message ->
    #  process_message(event.message)
    # Iris.Model.Channel.Create ->
    #  create_channel(event.message)
    # _ ->
    :ok
    # end
  end

  # defp create_channel(create_channel) do
  #  case Channel.read!(create_channel.id) do
  #    nil ->
  #      %Channel{
  #        id:         create_channel.id,
  #        name:       create_channel.name,
  #        owner:      create_channel.sender_id,
  #        members:    create_channel.invited_ids,
  #        created_ts: Iris.Util.now_to_utc(:os.timestamp())
  #      }
  #      |> Iris.Database.Channel.write!
  #    _ ->
  #      :error
  #  end
  # end

  # @spec process_message(%Iris.Model.Message{}) :: term
  # defp process_message(message) do
  #  case Channel.read!(message.channel_id) do
  #    nil ->
  #      :drop
  #    channel ->
  #      %{channel | last_message: message, last_ts: message.created_ts}
  #      |> Channel.write!
  #      append_message(message)
  #  end
  # end

  # defp append_message(message) do
  #  case Messages.read!(message.channel_id) do
  #    nil ->
  #      %Messages{channel_id: message.channel_id, messages: [message]}
  #      |> Messages.write!
  #    messages ->
  #      %{messages | messages: messages.messages ++ [message]}
  #      |> Messages.write!
  #  end
  # end
end
