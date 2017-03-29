defmodule Iris.Client do
  @behaviour :gen_fsm

  def start_link(ws_pid) do
    :gen_fsm.start_link(__MODULE__, [ws_pid], [])
  end

  def init([ws_pid]) do
    Process.monitor(ws_pid)
    state = %{:socket => ws_pid}
    send_message(Iris.Message.hello(), state)
    {:ok, :connected, state}
  end

  def handle_info({:'DOWN', _ref, :process, pid, _reason}, state) do
    {:stop, :normal, state}
  end
  # TODO: kick_out
  def handle_info(_info, name, state) do
    {:next_state, name, state}
  end

  def handle_event(_event, name, state) do
    {:next_state, name, state}
  end

  def handle_sync_event(_event, _from, name, state) do
    {:reply, :ok, name, state}
  end

  def code_change(_oldvsn, name, state, _extra) do
    {:ok, name, state}
  end

  def terminate(_reason, _name, state) do
    # delete session
    :ok
  end

  def connected(%{"type" => "auth"} = event, state) do
    {:next_state, :established, state}
  end
  def connected(_event, state) do
    {:next_state, :connected, state}
  end

  def established(event, state) do
    # parse message
    {:next_state, :established, state}
  end

  def send_message(message, %{:socket => ws}) do
    send ws, {:text, Poison.encode(message)}
  end
end
