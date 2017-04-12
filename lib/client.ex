defmodule Iris.Client do
  require Logger

  @behaviour :gen_fsm

  def start_link(ws_pid) do
    :dbg.tracer
    :dbg.tpl(__MODULE__, [])
    :dbg.p(:all, :c)
    :gen_fsm.start_link(__MODULE__, [ws_pid], [])
  end

  def init([ws_pid]) do
    Process.monitor(ws_pid)
    state = %{:socket => ws_pid}
    send_message(Iris.Message.hello(), state)
    {:ok, :connected, state}
  end

  def handle_info({:'DOWN', _ref, :process, _pid, _reason}, state) do
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

  def terminate(reason, _name, state) do
    case reason do
      :normal ->
        Iris.Session.delete(state[:session])
      _ ->
        :ok
    end
  end

  def connected(%{"type" => "auth"} = event, state) do
    user = event["user"]
    case event["pass"] do
      ^user ->
        session = Iris.Session.save(self(), user)
        send_message(Iris.Message.session(session.id), state)
        state2 = Map.merge(state, %{user: user, session: session.id})
        {:next_state, :established, state2}
      _ ->
        send_message(Iris.Message.error("Password doesn't match"), state)
        {:next_state, :connected, state}
    end
  end
  def connected(_event, state) do
    {:next_state, :connected, state}
  end

  # TODO implement bye message
  def established(%{"type" => "bye"} = _event, state) do
    {:stop, :normal, state}
  end

  def send_message(message, %{:socket => ws}) do
    {:ok, text} = Poison.encode(message)
    send ws, {:text, text}
  end
end
