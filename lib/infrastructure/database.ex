use Amnesia

defdatabase Iris.Database do
  require Logger

  # Event store for channel aggregate
  deftable Channel, [:id, :changes] do
    @type t :: %Channel{
            id: String.t(),
            changes: list
          }
  end

  # Projection for inbox
  deftable Inbox, [:user_channel_id, :last_user_id, :last_message, :last_ts] do
    @type t :: %Inbox{
            user_channel_id: String.t(),
            last_user_id: String.t(),
            last_message: term(),
            last_ts: String.t()
          }

    def find_item!(user_id, channel_id) do
      Logger.info("Finding inbox user: #{user_id} channel: #{channel_id}")
      Inbox.read!({user_id, channel_id})
    end
  end

  def init do
    case System.get_env("OTHER_NODE") do
      nil ->
        Logger.info("Storing schema on disk")
        Amnesia.Table.copying(:schema, node(), :disk)
        Iris.Database.Channel.create()
        Iris.Database.Channel.copying(node(), :disk)
        Iris.Database.Inbox.create()
        Iris.Database.Inbox.copying(node(), :disk)

      "NO" ->
        :ok

      node_name ->
        node = String.to_atom(node_name)
        join(node)
    end
  end

  def id do
    :erlang.phash2({node(), System.os_time()}, 0xFFFFFFFF)
    |> Integer.to_string()
  end

  defp join(other_node) do
    extras = Amnesia.info(:extra_db_nodes)
    Logger.info(fn -> "mnesia nodes: #{extras}" end)

    if other_node not in extras do
      Logger.info(fn -> "Add #{other_node} to extra_db_nodes" end)

      {:ok, _} =
        :mnesia.change_config(
          :extra_db_nodes,
          [other_node | extras]
        )
    end

    Logger.info("Change schema table copy")

    case Amnesia.Table.copying(:schema, node(), :disk) do
      {:atomic, :ok} ->
        :ok

      {:aborted, {:already_exists, _, _, _}} ->
        :ok

      {:error, {:already_exists, _, _, _}} ->
        :ok

      error ->
        exit(error)
    end

    :rpc.call(other_node, Amnesia, :info, [:tables])
    |> Enum.filter(&(&1 != :schema))
    |> Enum.map(&{&1, table_storage(other_node, &1)})
    |> Enum.each(fn {table, storage} ->
      Logger.info(fn -> "Change #{table} to #{storage}" end)
      Amnesia.Table.add_copy(table, node(), storage)
    end)

    Logger.info(fn -> "Copying schema to #{other_node}" end)
    # Amnesia.Table.copying(:schema, other_node, :disk)
  end

  defp table_storage(other_node, table) do
    case :rpc.call(other_node, Amnesia.Table, :properties, [table]) do
      {:badrpc, error} ->
        exit(error)

      result ->
        {_, storage} = result |> List.keyfind(:storage, 0)
        storage
    end
  end
end
