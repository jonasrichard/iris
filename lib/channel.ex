defmodule Iris.Channel do
  alias Database.Channel, as: Channel

  def create(name, owner, members) do
    id = Database.id()
    now = System.system_time
    %Channel{id: id, name: name, owner: owner,
             members: members, created_ts: now, last_ts: now}
      |> Channel.write!
  end
end
