defmodule Iris.Debug do
  alias Amnesia.Table.Match

  def channels do
    with %Match{values: values} <- Iris.Database.Channel.match!([]), do: values
  end

  def inbox do
    with %Match{values: values} <- Iris.Database.Inbox.match!([]), do: values
  end

  def cmd_create_channel(id, name, sender_id) do
    %Iris.Command.CreateChannel{id: id, name: name, sender_id: sender_id}
    |> Iris.CommandDispatcher.send()
  end

  def cmd_send_message(id, sender_id, channel_id, body) do
    %Iris.Command.SendMessage{
      id: id,
      sender_id: sender_id,
      channel_id: channel_id,
      body: body,
      created_ts: Iris.Util.now_to_utc()
    }
    |> Iris.CommandDispatcher.send()
  end
end
