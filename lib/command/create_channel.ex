defmodule Iris.Command.CreateChannel do
  defstruct [:id, :name, :sender, :members, :first_message, :ts]

  def new(name, sender, members, first_message) do
    %Iris.Command.CreateChannel{
      id: UUID.uuid4(),
      name: name,
      sender: sender,
      members: members,
      first_message: first_message,
      ts: Iris.Util.now_to_utc()
    }
  end
end
