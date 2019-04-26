defmodule Iris.Command do
  defmodule CreateChannel do
    defstruct [:id, :name, :sender, :members, :first_message, :ts]

    def new(name, sender, members, first_message) do
      %CreateChannel{
        id: UUID.uuid4(),
        name: name,
        sender: sender,
        members: members,
        first_message: first_message,
        ts: Iris.Util.now_to_utc()
      }
    end
  end

  defmodule SendMessage do
    defstruct [:id, :sender, :channel, :body, :ts]

    def new(sender, channel, body) do
      %SendMessage{
        id: UUID.uuid4(),
        sender: sender,
        channel: channel,
        body: body,
        ts: Iris.Util.now_to_utc()
      }
    end
  end

  defmodule ReceiveMessage do
    defstruct [:id, :receiver, :channel, :message_id, :ts]
  end

  defmodule ReadChannel do
    defstruct [:id, :reader, :channel, :ts]
  end

  # TODO add message_id to messages!

  # Edit message text

  # Add new user to channel
  # Drop somebody from the channel
end
