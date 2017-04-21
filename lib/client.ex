defmodule Iris.Client do
  require Logger

  alias Database.Channel, as: Channel
  alias Database.UserChannel, as: UserChannel

  @behaviour :gen_fsm

  def start_link(ws_pid) do
    :gen_fsm.start_link(__MODULE__, [ws_pid], [])
  end

  def init([ws_pid]) do
    Process.monitor(ws_pid)
    state = %{socket: ws_pid, channels: %{}}
    send_message(Iris.Message.hello(), state)
    {:ok, :connected, state}
  end

  def handle_info({:route, message}, name, state) do
    send_message(message, state)
    {:next_state, name, state}
  end
  def handle_info({:'DOWN', _ref, :process, _pid, _reason}, _name, state) do
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

  def connected(%{type: "auth"} = event, state) do
    user = event[:user]
    case event[:pass] do
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
  def connected(event, state) do
    Logger.debug("Got an unknown message #{event}")
    {:next_state, :connected, state}
  end

  def established(%{type: "channel.create"} = event, state) do
    state2 = handle_create_channel(event, state)
    {:next_state, :established, state2}
  end
  def established(%{type: "channel.list"}, state) do
    handle_channel_list(state)
    {:next_state, :esbablished, state}
  end
  def established(%{type: "bye"} = _event, state) do
    {:stop, :normal, state}
  end

  defp send_message(message, %{:socket => ws}) do
    {:ok, text} = Poison.encode(message)
    send ws, {:text, text}
  end

  defp handle_create_channel(msg, state) do
    channel = Iris.Channel.create(msg[:name], state[:user], msg[:invitees])
    {:ok, pid} = Iris.Channel.ensure_channel(channel)
    Iris.Channel.notify_create(pid, channel)
    cache_channel_pid(state, channel.id, pid)
  end

  defp handle_channel_list(state) do
    channels =
      case UserChannel.read!(state[:user]) do
        nil ->
          []
        uc ->
          uc.channel_ids
          |> Enum.map(&(Channel.read!(&1)))
      end
    message = %{type: "channel.list",
                channels: channels}
    send_message(message, state)
  end

  defp cache_channel_pid(state, channel_id, pid) do
    state
    |> Map.update(:channels, %{channel_id => pid},
         fn(channels) -> Map.put(channels, channel_id, pid) end)
  end
end
