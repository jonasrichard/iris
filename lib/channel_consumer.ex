defmodule Iris.Consumer.Channel do
  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message

  require Logger

  def handle_message_set(message_set, state) do
    for %Message{value: message} <- message_set do
      event = json_to_struct(message)
      Logger.info(fn -> "message: " <> inspect(event) end)
      # TODO dynamic registration
      Iris.Projection.Inbox.apply(event)
    end
    {:async_commit, state}
  end

  defp json_to_struct(json) do
    map = Jason.decode!(json, keys: :atoms)
    type = map.__struct__ |> String.to_atom()
    Map.put(map, :__struct__, type)
  end
end
