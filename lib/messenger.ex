defmodule Iris.Messenger do
  use GenServer

  require Logger

  # High-level API

  def open(name, pass) do
    {:ok, pid} = start_link()
    {:ok, hello} = recv_msg(pid)
    %{"type" => "hello"} = hello
    send_msg(pid, Iris.Message.auth(name, pass))
    {:ok, session} = recv_msg(pid)
    {:ok, pid, session[:id]}
  end

  # Low-level API

  def start_link do
    GenServer.start_link(__MODULE__, [self()], [])
  end

  def recv_msg(pid) do
    c = {make_ref(), self()}
    send(pid, {:recv, c})

    receive do
      {:msg, ^c, message} ->
        {:ok, message}
    after
      4_000 ->
        send(pid, {:un_recv, c})

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
    {:ok, conn} = :gun.open('localhost', 8080)
    Process.monitor(conn)
    {:ok, %{parent: parent, pending: [], messages: [], conn: conn, not_ready: true}}
  end

  def handle_info({:recv, from}, state) do
    case state[:messages] do
      [] ->
        {:noreply, append(state, :pending, from)}

      [message | rest] ->
        {_, client} = from
        send(client, {:msg, from, message})
        send_read(state, message)
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
        Logger.debug(fn -> "#{state[:user]} received message #{inspect(json)}" end)
        {:noreply, handle_message(json, state)}

      _ ->
        Logger.warn(fn -> "Cannot parse json #{text}" end)
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

  def handle_info({:gun_ws, _pid, {:close, _, _}}, state) do
    {:stop, :normal, state}
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    {:stop, :normal, state}
  end

  def handle_call({:send, message}, _from, state) do
    state2 =
      case message do
        %{type: "auth"} ->
          Map.put(state, :user, message[:user])

        _ ->
          state
      end

    send_message(state2, message)
    {:reply, :ok, state2}
  end

  defp handle_message(message, state) do
    send_received(state, message)

    case state[:pending] do
      [] ->
        append(state, :messages, message)

      [{_, client} = from | rest] ->
        # Check if there is older messages we got
        case state[:messages] do
          [] ->
            send(client, {:msg, from, message})
            send_read(state, message)
            Map.put(state, :pending, rest)

          [msg | rest_msgs] ->
            send(client, {:msg, from, msg})
            send_read(state, msg)

            state
            |> Map.put(:messages, rest_msgs ++ [message])
            |> Map.put(:pending, rest)
        end
    end
  end

  defp send_received(
         %{user: from} = state,
         %{"type" => "message", "subtype" => "incoming", "from" => to} = message
       ) do
    received =
      message
      |> Map.put("from", from)
      |> Map.put("to", to)
      |> Map.put("subtype", "received")
      |> Map.delete("text")

    send_message(state, received)
  end

  defp send_received(state, _) do
    state
  end

  defp send_read(
         %{user: from} = state,
         %{"type" => "message", "subtype" => "incoming", "from" => to} = message
       ) do
    read =
      message
      |> Map.put("from", from)
      |> Map.put("to", to)
      |> Map.put("subtype", "read")
      |> Map.delete("text")

    send_message(state, read)
  end

  defp send_read(state, _) do
    state
  end

  defp send_message(%{conn: conn} = state, message) do
    Logger.debug(fn -> "#{state[:user]} sending message #{inspect(message)}" end)
    {:ok, json} = Poison.encode(message)
    :gun.ws_send(conn, {:text, json})
  end

  defp append(map, key, value) do
    Map.update(map, key, [value], &(&1 ++ [value]))
  end
end
