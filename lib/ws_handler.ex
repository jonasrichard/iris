defmodule Iris.WSHandler do

  def init(req, _state) do
    {:cowboy_websocket, req, %{}}
  end

  def websocket_init(state) do
    {:ok, pid} = Iris.ClientSup.start_child(self())
    {:ok, Map.put(state, :client, pid)}
  end

  def websocket_handle({:text, json}, state) do
    case Poison.decode(json) do
      {:ok, map} ->
        :gen_fsm.send_event(state[:client], map)
        {:ok, state}
      {:error, _reason} ->
        {:reply, {:text, "Invalid message"}, state}
    end
  end
  def webosocket_handle(_, state) do
    {:ok, state}
  end

  def websocket_info({:text, _} = frame, state) do
    {:reply, frame, state}
  end
  def websocket_info(_, state) do
    {:ok, state}
  end
end
