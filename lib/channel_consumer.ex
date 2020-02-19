defmodule Iris.Consumer.Channel do
  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message

  require Logger

  def handle_message_set(message_set, state) do
    for %Message{value: message} <- message_set do
      event = Iris.Util.json_to_struct(message)
      Logger.info(fn -> "message: " <> inspect(event) end)
      # TODO dynamic registration
      # try do
      Iris.Projection.Inbox.apply(event)
      # catch
      #  type, reason ->
      #    Logger.error("#{type} #{inspect reason}")
      # end
    end

    {:async_commit, state}
  end
end
