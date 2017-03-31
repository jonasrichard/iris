defmodule Iris.Messenger do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [self()], [])
  end

  def init([parent]) do
    {:ok, conn} = :gun.open('localhost', 8080)
    {:ok, %{parent: parent,
            pending: [],
            conn: conn,
            not_ready: true}}
  end

  def handle_info({:gun_up, pid, :http}, state) do
    :gun.ws_upgrade(pid, "/ws")
    {:noreply, state}
  end
  def handle_info({:gun_ws_upgrade, pid, :ok, _}, state) do
    {:noreply, Map.delete(state, :not_ready)}
  end
end
