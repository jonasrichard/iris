defmodule Iris.Client.State do
  defstruct [:socket, :session, :user, channels: %{}]
end

defmodule Iris.Client do
  require Logger

  alias Database.Channel, as: Channel
  alias Database.UserChannel, as: UserChannel
  alias Iris.Client.State, as: State
  alias Iris.Message, as: Message

  @behaviour :gen_fsm

  def start_link(ws_pid) do
    :gen_fsm.start_link(__MODULE__, [ws_pid], [])
  end

  def init([ws_pid]) do
    Process.monitor(ws_pid)

    state =
      %State{socket: ws_pid}
      |> send_message(Iris.Message.hello())

    {:ok, :connected, state}
  end

  def handle_info({:route, message}, name, state) do
    state
    |> send_message(message)
    |> next(name)
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, _name, state) do
    {:stop, :normal, state}
  end

  def handle_info(:kick_out, _name, state) do
    Logger.info("User is kicked out")
    {:stop, :normal, state}
  end

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
        Iris.Session.delete(state.session)

      _ ->
        :ok
    end
  end

  def connected(%{type: "auth"} = event, state) do
    user = event[:user]

    case event[:pass] do
      ^user ->
        session = Iris.Session.save(self(), user)

        state
        |> send_message(Iris.Message.session(session.id))
        |> Map.merge(%{user: user, session: session.id})
        |> next(:established)

      _ ->
        state
        |> send_message(Iris.Message.error("Password doesn't match"))
        |> next(:connected)
    end
  end

  def connected(event, state) do
    Logger.debug(fn -> "Got an unknown message #{event}" end)
    {:next_state, :connected, state}
  end

  def established(%{type: "message"} = event, state) do
    case event[:subtype] do
      "send" ->
        event2 = Map.put(event, :ts, Message.ts())
        handle_message_send(state, event2)

      "received" ->
        handle_message_received(state, event)

      "read" ->
        handle_message_read(state, event)
    end
  end

  def established(%{type: "channel.history"} = event, state) do
    state
    |> handle_channel_history(event)
    |> next(:established)
  end

  def established(%{type: "channel.status"} = event, state) do
    state
    |> handle_channel_status(event)
    |> next(:established)
  end

  def established(%{type: "channel.create"} = event, state) do
    state
    |> handle_create_channel(event)
    |> next(:established)
  end

  def established(%{type: "channel.list"}, state) do
    state
    |> handle_channel_list()
    |> next(:established)
  end

  def established(%{type: "bye"} = _event, state) do
    state
    |> send_message(Message.bye())
    |> stop()
  end

  defp next(state, state_name) do
    {:next_state, state_name, state}
  end

  defp stop(state) do
    {:stop, :normal, state}
  end

  defp send_message(%State{socket: ws} = state, message) do
    {:ok, text} = Poison.encode(message)
    send(ws, {:text, text})
    state
  end

  defp handle_channel_history(state, %{channel: channel_id}) do
    messages =
      channel_id
      |> Iris.History.read_history()
      |> Enum.map(&Message.message_archive(&1))

    state
    |> send_message(Message.channel_history(channel_id, messages))
  end

  defp handle_create_channel(%State{user: user} = state, msg) do
    invitees = Enum.filter(msg[:invitees], &(&1 != user))
    channel = Iris.Channel.create(msg[:name], user, invitees)
    {state2, pid} = get_channel_pid(state, channel.id)
    Iris.Channel.notify_create(pid, channel)
    state2
  end

  defp handle_channel_list(state) do
    channels =
      case UserChannel.read!(state.user) do
        nil ->
          []

        uc ->
          uc.channel_ids
          |> Enum.map(&Channel.read!(&1))
      end

    message = Message.channel_list(channels)
    send_message(state, message)
  end

  defp handle_channel_status(state, _msg) do
    # TODO implement the read cursor list for a channel
    state
  end

  defp handle_message_send(state, %{channel: channel_id} = event) do
    {state2, pid} = get_channel_pid(state, channel_id)
    event2 = Map.put(event, :user, state2.user)
    Iris.Channel.message_broadcast(pid, event2)
    next(state2, :established)
  end

  defp handle_message_received(state, msg) do
    # TODO move it to channel process
    %{channel: ch_id, from: from, to: to, ts: ts} = msg
    received = Message.message_received(ch_id, from, to, ts)
    Iris.Session.send_message(to, received)
    next(state, :established)
  end

  defp handle_message_read(state, msg) do
    %{channel: ch_id, from: from, to: to, ts: ts} = msg
    {state2, pid} = get_channel_pid(state, ch_id)
    Iris.Channel.message_read(pid, from, ts, to)
    next(state2, :established)
  end

  defp cache_channel_pid(state, channel_id, pid) do
    %State{state | channels: Map.put(state.channels, channel_id, pid)}
  end

  defp get_channel_pid(%State{channels: channels} = state, channel_id) do
    case channels[channel_id] do
      nil ->
        {:ok, pid} = Iris.Channel.ensure_channel_by_id(channel_id)
        state2 = cache_channel_pid(state, channel_id, pid)
        {state2, pid}

      pid ->
        {state, pid}
    end
  end
end
