defmodule Iris.Database2 do
  defmodule Channel do
    require Logger

    def read!(id) do
      cmd = "SELECT version, change FROM iris.channel WHERE id = '#{id}'"
      conn = Process.get(:connection)
      {:ok, result} = Xandra.execute(conn, cmd)
      Logger.info("channel write: #{inspect(result)}")
    end

    def write!(id, version, change) do
      cmd =
        "INSERT INTO iris.channel (id, version, change) VALUES ('#{id}', #{version}, '#{change}')"

      conn = Process.get(:connection)
      {:ok, result} = Xandra.execute(conn, cmd)
      Logger.info("channel write: #{inspect(result)}")
    end
  end
end
