defmodule Iris.Aggregate do
  defmodule Channel do
    defstruct [:id, :name, :owner, :members]

    @doc "Load and reconstruct aggregate by applying changes"
    def load(id) do
      case Iris.Database.Channel.read!(id) do
        nil ->
          nil

        item ->
          item
          |> to_channel()
      end
    end

    # TODO Does the channel need to check if it already exists?
    # What is its responsibility?
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

    def send_message(channel, id, sender, body, ts) do
      %Iris.Event.MessageSent{
        id: UUID.uuid4(),
        # message_id needed
        sender: sender,
        channel: channel.id,
        body: body,
        ts: ts,
        members: channel.members
      }
      |> append_events(id)
    end

    defp to_channel(db_item) do
      Enum.reduce(db_item.changes, %Channel{id: db_item.id}, &apply_change/2)
    end

    defp apply_change(_change = %Iris.Event.MessageSent{}, acc) do
      acc
    end

    defp apply_change(change = %Iris.Event.ChannelCreated{}, acc) do
      %{acc | id: change.channel, name: change.name, owner: change.owner, members: change.members}
    end

    @doc "Append event or list of events to a channel and dispatch the events"
    def append_events(event, id) do
      case Iris.Database.Channel.read!(id) do
        nil when is_list(event) ->
          %Iris.Database.Channel{id: id, changes: event}
          |> Iris.Database.Channel.write!()

        nil ->
          %Iris.Database.Channel{id: id, changes: [event]}
          |> Iris.Database.Channel.write!()

        channel when is_list(event) ->
          %{channel | changes: event ++ channel.changes}
          |> Iris.Database.Channel.write!()

        channel ->
          %{channel | changes: [event | channel.changes]}
          |> Iris.Database.Channel.write!()
      end

      Iris.EventDispatcher.send(event)
    end
  end
end
