defmodule Iris.Messenger do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [self()], [])
  end

  def init([parent]) do
    :dbg.tracer
    :dbg.tpl(Iris.Messenger, [])
    :dbg.p(:all, :c)
    {:ok, conn} = :gun.open('localhost', 8080)
    {:ok, %{parent: parent,
            pending: [],
            conn: conn,
            not_ready: true}}
  end

  def handle_info({:gun_ws, _, {:text, text}}, state) do
    {:noreply, Poison.decode(text) |> handle_message(state)}
  end
  def handle_info({:gun_up, pid, :http}, state) do
    :gun.ws_upgrade(pid, '/ws')
    {:noreply, state}
  end
  def handle_info({:gun_ws_upgrade, pid, :ok, _}, state) do
    {:noreply, Map.delete(state, :not_ready)}
  end
  def handle_info({:gun_down, pid, :http, _, _, _}, state) do
    {:stop, :normal, state}
  end

  defp handle_message(_message, state) do
    state
  end
end
