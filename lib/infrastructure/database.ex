defmodule Iris.Database do
  def init() do
    # Cassandra only
    Iris.Cassandra.create_namespace()
    Iris.Cassandra.Channel.create_table()
    Iris.Cassandra.Inbox.create_table()
  end

  # In case of more event sourced data, we need to have a selector here
  def append_changes(id, event, version) do
    Iris.Database.Channel.write!(id, version, event)
  end

  defmodule Channel do
    defstruct [:id, :changes]

    @spec read(String.t()) :: %Iris.Database.Channel{} | nil
    def read(id) do
      case Iris.Cassandra.Channel.read!(id) do
        [] ->
          nil

        changes ->
          %Iris.Database.Channel{id: id, changes: changes}
      end
    end

    @spec write!(String.t(), number, struct) :: any
    def write!(id, version, change) do
      Iris.Cassandra.Channel.write!(id, version, change)
    end
  end

  defmodule Inbox do
    defstruct [:user_id, :channel_id, :last_user_id, :last_message, :last_ts]

    def get_inbox(user_id, channel_id) do
      Iris.Cassandra.Inbox.read!(user_id, channel_id)
    end

    def get_inbox_by_user_id(user_id) do
      Iris.Cassandra.Inbox.read!(user_id)
    end

    def write!(user_id, channel_id) do
      Iris.Cassandra.Inbox.write!(user_id, channel_id)
    end

    def write!(user_id, channel_id, last_user_id, last_message, last_ts) do
      Iris.Cassandra.Inbox.write!(user_id, channel_id, last_user_id, last_message, last_ts)
    end
  end
end
