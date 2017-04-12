defmodule Iris.WSHandler do

  def init(req, _state) do
    {:cowboy_websocket, req, %{}}
  end

  def websocket_init(state) do
    {:ok, client} = Iris.ClientSup.start_child(self())
    {:ok, Map.put(state, :client, client)}
  end

  def websocket_handle({:text, json}, state) do
    case Poison.decode(json) do
      {:ok, map} ->
        case Iris.Message.parse(map) do
          {:ok, msg} ->
            :gen_fsm.send_event(state[:client], msg)
            {:ok, state}
          {:error, "Unknown message"} ->
            # TODO send structured error
            {:reply, {:text, "Unknown message"}, state}
        end
      {:error, _reason} ->
        {:reply, {:text, "Cannot parse json"}, state}
    end
  end
  def websocket_handle(_, state) do
    {:ok, state}
  end

  def websocket_info({:text, _} = frame, state) do
    {:reply, frame, state}
  end
  def websocket_info(_, state) do
    {:ok, state}
  end
end
