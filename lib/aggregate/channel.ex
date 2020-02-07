defmodule Iris.Aggregate.Channel do
  defstruct [:id, :name, :owner, :members, :last_version]

  @doc "Load and reconstruct aggregate by applying changes"
  @spec load(String.t) :: %Iris.Aggregate.Channel{} | nil
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
        sender: owner,
        channel: id,
        body: first_message,
        ts: ts,
        members: members
      }
    ]
    |> append_events(id)
  end

  def send_message(channel, id, message_id, sender, body, ts) do
    %Iris.Event.MessageSent{
      id: UUID.uuid4(),
      message_id: message_id,
      sender: sender,
      channel: channel.id,
      body: body,
      ts: ts,
      members: channel.members
    }
    |> append_events(id)
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
        |> append_events(channel.id)

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
        |> append_events(channel.id)

      false ->
        # we can say a better reason
        {:error, :cannot_be_kicked}
    end
  end

  defp to_channel(db_item) do
    #last_version = db_item
    #               |> Enum.map(&(&1.version))
    #               |> Enum.max()
    Enum.reduce(db_item.changes, %Iris.Aggregate.Channel{id: db_item.id}, &apply_change/2)
    #|> Map.put(:last_version, last_version)
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

  @doc "Append event or list of events to a channel and dispatch the events"
  defp append_events(events, id) do
    for event <- events do
      append_event(event, id, 0)
    end
    events
  end

  defp append_event(event, id, version) do
    Iris.Database.Channel.write!(id, version, event)
    _partition = Iris.Util.uuid_to_partition(id)
    Iris.EventDispatcher.dispatch(0, event)
  end

    # TODO implement the uncommitted changes part!
end
