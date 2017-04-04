defmodule Iris.Mnesia do
  require Logger

  def wait_for do
    ensure_schema()
    ensure_session()
  end

  defp ensure_schema do
    case :mnesia.system_info(:extra_db_nodes) do
      [] ->
        Application.stop(:mnesia)
        node = node()
        Logger.info("Creating schema on node")
        case :mnesia.create_schema([node]) do
          :ok ->
            Application.start(:mnesia, :permanent)
          {:error, {^node, {:already_exists, ^node}}} ->
            Application.start(:mnesia, :permanent)
          error ->
            exit(error)
        end
      extras ->
        Logger.info("mnesia already has extra db nodes: #{extras}")
    end
  end

  defp ensure_session do
    :ok = ensure_table(:session, [{:ram_copies, [node()]},
                                  {:index, [3]},
                                  {:record_name, :session},
                                  {:attributes, [:id, :user, :pid]}])
  end

  defp ensure_table(table, tabledef) do
    case :mnesia.create_table(table, tabledef) do
      {:atomic, :ok} ->
        :ok
      {:aborted, {:already_exists, ^table}} ->
        :ok
      other ->
        other
    end
  end

  def join(other_node) do
    extras = :mnesia.system_info(:extra_db_nodes)
    Logger.info("mnesia nodes: #{extras}")

    if other_node in extras do
      Logger.info("Add #{other_node} to extra_db_nodes")
      {:ok, _} = :mnesia.change_config(:extra_db_nodes, [other_node | extras])
    end

    Logger.info("Change schema table copy")
    case :mnesia.change_table_copy_type(:schema, node(), :disc_copies) do
      {:atomic, :ok} ->
        :ok
      {:aboreted, {:already_exists, _, _, _}} ->
        :ok
      error ->
        exit(error)
    end

    :mnesia.system_info(:tables)
    |> Enum.filter(&(&1 != :schema))
    |> Enum.map(&({&1, table_type(other_node, &1)}))
    |> Enum.each(fn {table, type} ->
                   Logger.info("Change #{table} to #{type}")
                   :mnesia.add_table_copy(table, node(), type)
                 end)
  end

  defp table_type(other_node, table) do
    case :rpc.call(other_node, :mnesia, :table_info, [table, :storage_type]) do
      {:badrpc, error} ->
        exit(error)
      result ->
        result
    end
  end
end
