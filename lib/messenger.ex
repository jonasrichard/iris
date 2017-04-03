defmodule Iris.Messenger do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [self()], [])
  end

  def recv_msg(pid) do
    c = {make_ref(), self()}
    send pid, {:recv, c}
    receive do
      {:msg, ^c, message} ->
        {:ok, message}
    after
      4_000 ->
        send pid, {:un_recv, c}
        receive do
          {:msg, ^c, message2} ->
            {:ok, message2}
        after
          1_000 ->
            {:error, :timeout}
        end
    end
  end

  def send_msg(pid, message) do
    GenServer.call(pid, {:send, message})
  end

  def init([parent]) do
    #:dbg.tracer
    #:dbg.tpl(Iris.Messenger, [])
    #:dbg.tpl(:gun_ws, [])
    #:dbg.p(:all, :c)
    {:ok, conn} = :gun.open('localhost', 8080)
    Process.monitor(conn)
    {:ok, %{parent: parent,
            pending: [],
            messages: [],
            conn: conn,
            not_ready: true}}
  end

  def handle_info({:recv, from}, state) do
    case state[:messages] do
      [] ->
        {:noreply, append(state, :pending, from)}
      [message | rest] ->
        {_, client} = from
        send client, {:msg, from, message}
        {:noreply, Map.put(state, :messages, rest)}
    end
  end
  def handle_info({:un_recv, from}, state) do
    pending2 = state[:pending] -- [from]
    {:noreply, Map.put(state, :pending, pending2)}
  end
  def handle_info({:gun_ws, _, {:text, text}}, state) do
    case Poison.decode(text) do
      {:ok, json} ->
        {:noreply, handle_message(json, state)}
      _ ->
        {:noreply, state}
    end
  end
  def handle_info({:gun_up, pid, :http}, state) do
    :gun.ws_upgrade(pid, '/ws')
    {:noreply, state}
  end
  def handle_info({:gun_ws_upgrade, _pid, :ok, _}, state) do
    {:noreply, Map.delete(state, :not_ready)}
  end
  def handle_info({:gun_down, _pid, :http, _, _, _}, state) do
    {:stop, :normal, state}
  end
  def handle_info({:'DOWN', _ref, :process, _pid, _reason}, state) do
    {:stop, :normal, state}
  end

  def handle_call({:send, message}, _from, state) do
    {:ok, json} = Poison.encode(message)
    :gun.ws_send(state[:conn], {:text, json})
    {:reply, :ok, state}
  end

  defp handle_message(message, state) do
    case state[:pending] do
      [] ->
        append(state, :messages, message)
      [{_, client} = from | rest] ->
        send client, {:msg, from, message}
        Map.put(state, :pending, rest)
    end
  end

  defp append(map, key, value) do
    Map.update(map, key, [value], &(&1 ++ [value]))
  end
end
