defmodule Iris.Aggregate do
  defmodule Channel do
    defstruct [:id, :name, :owner]

    def load(id) do
      case Iris.Database.Channel.read!(id) do
        nil ->
          # TODO in this case who stores the channel in event store?
          # probably we need to store it here, because we know exactly
          # that the channel doesn't exist
          %Channel{id: id}

        item ->
          item
          |> to_channel()
      end
    end

    def apply(channel, %Iris.Event.MessageSent{} = event) do
      # TODO iterate over the members!
      [
        %Iris.Event.InboxMessageArrived{
          user_id:    event.sender_id,  # this will be the member_id
          channel_id: channel.id,
          sender_id:  event.sender_id,
          body:       event.body,
          message_ts: event.created_ts
        }
      ]
    end
    def apply(_channel, %Iris.Event.ChannelCreated{} = _event) do
      # The changes are already stored in the event store, now we just need
      # to send the resulted events
      nil
    end

    defp to_channel(db_item) do
      Enum.reduce(db_item.changes, %Channel{id: db_item.id}, &apply_change/2)
    end

    defp apply_change(change = %Iris.Event.MessageSent{}, acc) do
      # %{acc | name: "Name", owner: change.sender_id}
      acc
    end

    defp apply_change(change = %Iris.Event.ChannelCreated{}, acc) do
      %{acc | id: change.channel_id, name: change.name, owner: change.sender_id}
    end
  end
end
