defmodule Iris.Consumer.Channel do
  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message

  require Logger

  def handle_message_set(message_set, state) do
    for %Message{value: message} <- message_set do
      event = Iris.Util.json_to_struct(message)
      Logger.info(fn -> "message: " <> inspect(event) end)
      # TODO dynamic registration
      Iris.Projection.Inbox.apply(event)
    end

    {:async_commit, state}
  end
end
