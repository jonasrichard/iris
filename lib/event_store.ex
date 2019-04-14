defmodule Iris.EventStore do
  alias Iris.Database.Channel, as: Channel

  def append_event(:channel, id, event) do
    case Channel.read!(id) do
      nil ->
        %Channel{id: id, changes: [event]}
        |> Channel.write!()

      channel ->
        %{channel | changes: [event | channel.changes]}
        |> Channel.write!()
    end
    Iris.EventDispatcher.send(event)
  end
end
