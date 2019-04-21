defmodule Iris.Debug do
  alias Amnesia.Table.Match

  def channels do
    with %Match{values: values} <- Iris.Database.Channel.match!([]), do: values
  end

  def inbox do
    with %Match{values: values} <- Iris.Database.Inbox.match!([]), do: values
  end

  def cmd_create_channel(name, sender_id, members, first_message) do
    Iris.Command.CreateChannel.new(name, sender_id, members, first_message)
    |> Iris.CommandDispatcher.send()
  end

  def cmd_send_message(sender_id, channel_id, body) do
    Iris.Command.SendMessage.new(sender_id, channel_id, body)
    |> Iris.CommandDispatcher.send()
  end
end
