defmodule Iris.Cassandra do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def query(query) do
    GenServer.call(__MODULE__, {:query, query})
  end

  def init(_) do
    {:ok, conn} = Xandra.start_link(nodes: [Application.fetch_env!(:iris, :database)[:host]])
    {:ok, %{:connection => conn}}
  end

  def handle_call({:query, query}, _from, state) do
    result = Xandra.execute(state[:connection], query)
    {:reply, result, state}
  end

  def create_namespace() do
    namespace = """
    CREATE KEYSPACE IF NOT EXISTS iris
      WITH replication = {'class':'SimpleStrategy','replication_factor':1};
    """

    {:ok, _} = query(namespace)
  end

  defmodule Channel do
    require Logger

    def create_table() do
      channel = """
      CREATE TABLE IF NOT EXISTS iris.channel (
        id varchar,
        version int,
        change varchar,
        primary key (id, version)
      );
      """

      {:ok, _} = Iris.Cassandra.query(channel)
    end

    def read!(id) do
      cmd = "SELECT version, change FROM iris.channel WHERE id = '#{id}'"
      {:ok, result} = Iris.Cassandra.query(cmd)
      Enum.map(result, fn %{"version" => version, "change" => change} ->
        {version, Iris.Util.json_to_struct(change)}
      end)
    end

    def write!(id, version, change) do
      change_json = Iris.Util.struct_to_json(change)
      cmd =
        "INSERT INTO iris.channel (id, version, change) VALUES ('#{id}', #{version}, '#{change_json}')"

      {:ok, result} = Iris.Cassandra.query(cmd)
      Logger.info("channel write: #{inspect(result)}")
    end
  end

  defmodule Inbox do
    # TODO find out how to order by last_ts desc (it is not a partition key)
    def create_table() do
      inbox = """
      CREATE TABLE IF NOT EXISTS iris.inbox (
        user_id varchar,
        channel_id varchar,
        last_user_id varchar,
        last_message varchar,
        last_ts timestamp,
        primary key (user_id, channel_id)
      );
      """
      {:ok, _} = Iris.Cassandra.query(inbox)
    end

    def read!(user_id) do
      cmd = """
      SELECT user_id, channel_id, last_user_id, last_message, last_ts
      FROM iris.inbox
      WHERE user_id = '#{user_id}'
      """
      {:ok, result} = Iris.Cassandra.query(cmd)
      Enum.map(result, &row_to_struct/1)
    end

    def read!(user_id, channel_id) do
      cmd = """
      SELECT user_id, channel_id, last_user_id, last_message, last_ts
      FROM iris.inbox
      WHERE user_id = '#{user_id}' AND channel_id = '#{channel_id}'
      """
      {:ok, result} = Iris.Cassandra.query(cmd)
      case Enum.map(result, &row_to_struct/1) do
        [item] ->
          item
        [] ->
          nil
      end
    end

    def write!(user_id, channel_id) do
      cmd = "INSERT INTO iris.inbox (user_id, channel_id) VALUES ('#{user_id}', '#{channel_id}')"
      {:ok, _} = Iris.Cassandra.query(cmd)
    end

    def write!(user_id, channel_id, last_user_id, last_message, last_ts) do
      cmd = """
        INSERT INTO iris.inbox (user_id, channel_id, last_user_id, last_message, last_ts) VALUES
          ('#{user_id}', '#{channel_id}', #{last_user_id}, #{last_message}, #{last_ts})
      """
      {:ok, _} = Iris.Cassandra.query(cmd)
    end

    def row_to_struct(map) do
      %Iris.Database.Inbox{
        user_id: map["user_id"],
        channel_id: map["channel_id"],
        last_user_id: map["last_user_id"],
        last_message: map["last_message"],
        last_ts: map["last_ts"]
      }
    end
  end
end
