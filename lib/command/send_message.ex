defmodule Iris.Command.SendMessage do
  defstruct [:id, :sender, :channel, :body, :ts]

  def new(sender, channel, body) do
    %Iris.Command.SendMessage{
      id: UUID.uuid4(),
      sender: sender,
      channel: channel,
      body: body,
      ts: Iris.Util.now_to_utc()
    }
  end
end
