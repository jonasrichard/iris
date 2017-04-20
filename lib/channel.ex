defmodule Iris.Channel do
  use GenServer

  alias Database.Channel, as: Channel
  alias Database.ChannelProc, as: ChannelProc

  @doc "Create channel in the mnesia database"
  def create(name, owner, members) do
    id = Database.id()
    now = System.system_time
    %Channel{id: id, name: name, owner: owner,
             members: [owner | members], created_ts: now, last_ts: now}
      |> Channel.write!
  end

  @doc "Ensure the channel pid is started"
  def ensure_channel(channel) do
    case ChannelProc.read!(channel.id) do
      nil ->
        Iris.ChannelSup.start_child(channel)
      pid ->
        {:ok, pid}
    end
  end

  def ensure_channel_by_id(id) do
    case Channel.read!(id) do
      nil ->
        {:error, :not_such_channel}
      channel ->
        ensure_channel(channel)
    end
  end

  def notify_create(pid, channel) do
    GenServer.call(pid, {:notify_create, channel})
  end

  def start_link(channel) do
    GenServer.start_link(__MODULE__, [channel], [])
  end

  def init([channel]) do
    %ChannelProc{channel_id: channel.id, pid: self()} |> ChannelProc.write!
    {:ok, %{id: channel.id, channel: channel}}
  end

  def handle_call({:notify_create, channel}, _from, state) do
    send_user(channel.owner, Iris.Message.channel_created(channel))
    invite = Iris.Message.channel_invited(channel)
    channel.members
    |> List.delete(channel.owner)
    |> Enum.each(fn member -> send_user(member, invite) end)
    {:reply, :ok, state}
  end

  defp send_user(user, message) do
    case Iris.Session.find_by_name(user) do
      nil ->
        # TODO offline messaging
        :ok
      [session] ->
        send session.pid, {:route, message}
    end
  end
end
