use Amnesia

defdatabase Database do
  require Logger

  deftable User, [:id, :name, :password]

  def init do
    case System.get_env("OTHER_NODE") do
      nil ->
        Logger.info("Storing schema on disk")
        Amnesia.Table.copying(:schema, node(), :disk)
      "NO" ->
        :ok
      node_name ->
        node = String.to_atom(node_name)
        join(node)
    end
  end

  defp join(other_node) do
    extras = Amnesia.info(:extra_db_nodes)
    Logger.info("mnesia nodes: #{extras}")

    if not other_node in extras do
      Logger.info("Add #{other_node} to extra_db_nodes")
      {:ok, _} = :mnesia.change_config(:extra_db_nodes,
                                       [other_node | extras])
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
    |> Enum.map(&({&1, table_storage(other_node, &1)}))
    |> Enum.each(fn {table, storage} ->
                   Logger.info("Change #{table} to #{storage}")
                   Amnesia.Table.add_copy(table, node(), storage)
                 end)
    Logger.info("Copying schema to #{other_node}")
    #Amnesia.Table.copying(:schema, other_node, :disk)
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
