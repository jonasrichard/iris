defmodule Iris.Aggregate.Channel do
  defstruct [:id, :name, :owner, :members, :last_version]

  require Logger

  @doc "Load and reconstruct aggregate by applying changes"
  @spec load(String.t()) :: %Iris.Aggregate.Channel{} | nil
  def load(id) do
    case Iris.Database.Channel.read(id) do
      nil ->
        nil

      [] ->
        nil

      item ->
        item
        |> to_channel()
    end
  end

  @spec create_channel(String.t(), String.t(), String.t(), list(), String.t(), String.t()) ::
          %Iris.Aggregate.Channel{}
  def create_channel(id, name, owner, members, first_message, ts) do
    [
      %Iris.Event.ChannelCreated{
        id: UUID.uuid4(),
        channel: id,
        name: name,
        owner: owner,
        members: members
      },
      %Iris.Event.MessageSent{
        id: UUID.uuid4(),
        message_id: UUID.uuid4(),
        sender: owner,
        channel: id,
        body: first_message,
        ts: ts,
        members: members
      }
    ]
    |> append_events(%Iris.Aggregate.Channel{id: id, last_version: 0})
  end

  def send_message(channel, id, message_id, sender, body, ts) do
    %Iris.Event.MessageSent{
      id: id,
      message_id: message_id,
      sender: sender,
      channel: channel.id,
      body: body,
      ts: ts,
      members: channel.members
    }
    |> append_event(channel)
  end

  def invite_user(channel, inviter, invitee, ts) do
    case invitee not in channel.members do
      true ->
        %Iris.Event.UserInvited{
          id: UUID.uuid4(),
          channel: channel.id,
          inviter: inviter,
          invitee: invitee,
          ts: ts,
          members: [invitee | channel.members]
        }
        |> append_event(channel)

      false ->
        {:error, :already_member}
    end
  end

  def kick_out_member(channel, kicker, kickee, ts) do
    case kickee in channel.members and kickee != channel.owner do
      true ->
        %Iris.Event.MemberKickedOut{
          id: UUID.uuid4(),
          channel: channel.id,
          kicker: kicker,
          kickee: kickee,
          ts: ts,
          members: List.delete(channel.members, kickee)
        }
        |> append_event(channel)

      false ->
        # we can say a better reason
        {:error, :cannot_be_kicked}
    end
  end

  defp to_channel(db_item) do
    last_version =
      db_item.changes
      |> Enum.map(&elem(&1, 0))
      |> Enum.max()

    db_item.changes
    |> Enum.reduce(
      %Iris.Aggregate.Channel{id: db_item.id},
      fn {version, event}, acc ->
        apply_change(event, acc)
        |> update_version(version)
      end
    )
    |> Map.put(:last_version, last_version)
  end

  defp apply_change(change = %Iris.Event.UserInvited{}, acc) do
    %{acc | members: [change.invitee | acc.members]}
  end

  defp apply_change(change = %Iris.Event.MemberKickedOut{}, acc) do
    %{acc | members: List.delete(acc.members, change.kickee)}
  end

  defp apply_change(change = %Iris.Event.ChannelCreated{}, acc) do
    %{acc | id: change.channel, name: change.name, owner: change.owner, members: change.members}
  end

  defp apply_change(_, acc) do
    acc
  end

  defp update_version(channel, version) do
    cond do
      channel.last_version < version ->
        %{channel | version: version}

      true ->
        channel
    end
  end

  defp append_events([], channel) do
    channel
  end

  defp append_events([event | rest], channel) do
    append_events(rest, append_event(event, channel))
  end

  # append_event
  #   1. Write the new event to the event store
  #   2. Dispatch the event
  #   3. Apply the change/event to the aggregate
  defp append_event(event, channel) do
    new_version = channel.last_version + 1
    Iris.Database.Channel.write!(channel.id, new_version, event)
    _partition = Iris.Util.uuid_to_partition(channel.id)
    Iris.EventDispatcher.dispatch(0, event)
    apply_change(event, Map.put(channel, :last_version, new_version))
  end

  # TODO implement the uncommitted changes part!
end
