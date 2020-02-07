defmodule Iris.Cassandra do

  def create_namespace() do
    namespace = """
    CREATE KEYSPACE IF NOT EXISTS iris
      WITH replication = {'class':'SimpleStrategy','replication_factor':1};
    """

    {:ok, _} = Xandra.execute(Process.get(:connection), namespace)
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

      {:ok, _} = Xandra.execute(Process.get(:connection), channel)
    end

    def read!(id) do
      cmd = "SELECT version, change FROM iris.channel WHERE id = '#{id}'"
      {:ok, result} = Xandra.execute(Process.get(:connection), cmd)
      Enum.map(result, fn %{"version" => version, "change" => change} ->
        {version, change}
      end)
    end

    def write!(id, version, change) do
      change_json = Iris.Util.struct_to_json(change)
      cmd =
        "INSERT INTO iris.channel (id, version, change) VALUES ('#{id}', #{version}, '#{change_json}')"

      {:ok, result} = Xandra.execute(Process.get(:connection), cmd)
      Logger.info("channel write: #{inspect(result)}")
    end
  end
end
