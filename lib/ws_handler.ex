defmodule Iris.Receiver.WSHandler do
  @behaviour :cowboy_websocket

  def init(req, state) do
    {:cowboy_websocket, req, state}
  end

  def websocket_init(state) do
    {:ok, client} = Iris.ClientSup.start_child(self())
    {:ok, Map.put(state, :client, client)}
  end

  def websocket_handle({:text, json}, state) do
    _ = Iris.Receiver.Main.handle(json)
    {:reply, state}
  end
  def websocket_handle(_, state) do
    {:reply, state}
  end

  def websocket_info({:text, _} = frame, state) do
    {:reply, frame, state}
  end
  def websocket_info(_, state) do
    {:reply, state}
  end

  def terminate(_reason, _req, _state) do
    :ok
  end
end

defmodule Iris.Receiver.Main do
  def handle(json) do
    case Decoder.decode(json) do
      {:ok, message} ->
        Iris.Event.Queue.store(message.id, message)
        {:ok, message}
      {:error, _} = error ->
        error
    end
  end
end

defmodule Iris.Receiver.Decoder do

  def decode(json) do
    case Jason.decode(json) do
      {:ok, obj} ->
        decode_by_type(obj)
      {:error, reason} ->
        :error
    end
  end

  defp decode_by_type(%{"type" => "message"} = json_map) do
    {:ok,
      %Iris.Model.Message{
        id: json_map["id"],
        sender_id: json_map["sender_id"],
        channel_id: json_map["channel_id"],
        body: json_map["body"],
        created_ts: json_map["created_ts"]
      }}
  end
  defp decode_by_type(%{"type" => type}) do
    {:error, "Unknown type #{type}"}
  end
end
