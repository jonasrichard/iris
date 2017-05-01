defmodule Iris.Channel do
  use GenServer
  require Logger

  alias Database.Channel, as: Channel
  alias Database.ChannelProc, as: ChannelProc
  alias Database.Cursor, as: Cursor
  alias Database.UserChannel, as: UserChannel
  alias Iris.Message, as: Message

  @doc "Create channel in the mnesia database"
  def create(name, owner, members) do
    id = Database.id()
    now = System.system_time
    %Channel{id: id,
             name: name,
             owner: owner,
             members: [owner | members],
             created_ts: now,
             last_ts: now}
    |> Channel.write!
  end

  @doc "Ensure the channel pid is started"
  def ensure_channel(channel) do
    case ChannelProc.read!(channel.id) do
      nil ->
        Iris.ChannelSup.start_child(channel)
      proc ->
        {:ok, proc.pid}
    end
  end

  def ensure_channel_by_id(id) do
    case Channel.read!(id) do
      nil ->
        {:error, :no_such_channel}
      channel ->
        ensure_channel(channel)
    end
  end

  @doc "Add the channel to the user channel list"
  def add_channel_to_user(user, channel_id) do
    case UserChannel.read!(user) do
      nil ->
        %UserChannel{user: user,
                     channel_ids: MapSet.new |> MapSet.put(channel_id)}
        |> UserChannel.write!
      uc ->
        %{uc | channel_ids: MapSet.put(uc.channel_ids, channel_id)}
        |> UserChannel.write!
    end
  end

  def notify_create(pid, channel) do
    GenServer.call(pid, {:notify_create, channel})
  end

  def message_broadcast(pid, message) do
    GenServer.call(pid, {:message_broadcast, message})
  end

  def message_read(pid, user, ts, to) do
    GenServer.call(pid, {:message_read, user, ts, to})
  end

  def start_link(channel) do
    GenServer.start_link(__MODULE__, [channel], [])
  end

  def init([channel]) do
    %ChannelProc{channel_id: channel.id, pid: self()} |> ChannelProc.write!
    {:ok, %{id: channel.id, channel: channel}}
  end

  def handle_call({:message_broadcast, message}, _from, state) do
    %{channel: channel_id, from: from, text: text, ts: ts} = message
    Iris.History.append_to_history(channel_id, message)
    # Broadcast messages
    incoming = Message.message_incoming(channel_id, from, text)
    state[:channel].members
    |> List.delete(from)
    |> Enum.each(fn member -> send_user(member, incoming) end)
    # Send stored to the sender
    send_user(from, Message.message_stored(channel_id, ts))
    {:reply, :ok, state}
  end
  def handle_call({:message_read, user, ts, to}, _from, state) do
    channel_id = state[:id]
    # Move cursor
    case Cursor.read!(channel_id) do
      nil ->
        %Cursor{channel_id: channel_id, read_pointers: %{user => ts}}
        |> Cursor.write!
      cursor ->
        cursor
        |> Map.update(:read_pointers, %{user => ts},
                      fn(pointers) -> Map.put(pointers, user, ts) end)
        |> Cursor.write!
    end
    # Send read receipt
    receipt = Message.message_read(channel_id, user, to, ts)
    send_user(to, receipt)
    {:reply, :ok, state}
  end
  def handle_call({:notify_create, channel}, _from, state) do
    # Notify owner that channel is created
    send_user(channel.owner, Message.channel_created(channel))
    # Add channel to everyone's channel list
    channel.members
    |> Enum.each(fn member -> add_channel_to_user(member, channel.id) end)
    # Notify invited members
    invite = Message.channel_invited(channel)
    channel.members
    |> List.delete(channel.owner)
    |> Enum.each(fn member -> send_user(member, invite) end)
    {:reply, :ok, state}
  end

  defp send_user(user, message) do
    Iris.Session.find_by_name(user)
    |> single(fn(session) -> send session.pid, {:route, message} end)
  end

  defp single(nil, _fun) do
    :ok
  end
  defp single(list, fun) when is_list(list) do
    case list do
      [] ->
        :ok
      [elem] ->
        fun.(elem)
      _ ->
        raise {:error, list}
    end
  end
  defp single(any, fun) do
    fun.(any)
  end
end
