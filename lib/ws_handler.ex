defmodule Iris.WSHandler do

  def init(req, _state) do
    {:cowboy_websocket, req, %{}}
  end

  def websocket_init(state) do
    # TODO start client with client_sup
    {:ok, state}
  end

  def websocket_handle({:text, json}, state) do
    case Poison.decode(json) do
      {:ok, _map} ->
        # pass it to the client
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
