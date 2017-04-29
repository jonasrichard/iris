use Amnesia

defdatabase Database do
  require Logger

  deftable User, [:id, :name, :password]
  deftable Session, [:id, :pid, :user], [index: [:user]]
  deftable Channel, [:id, :name, :owner, :members, :created_ts, :last_ts]
  deftable ChannelProc, [:channel_id, :pid]
  deftable Cursor, [:channel_id, :read_pointers]
  deftable UserChannel, [:user, :channel_ids]
  deftable History, [:channel_id, :start_ts, :last_ts, :messages, :index]
  deftable HistoryIndex, [:id, :messages, :start_ts, :last_ts]

  def init do
    case System.get_env("OTHER_NODE") do
      nil ->
        Logger.info("Storing schema on disk")
        Amnesia.Table.copying(:schema, node(), :disk)
        Database.User.create()
        Database.User.copying(node(), :disk)
        Database.Session.create()
        Database.Session.copying(node(), :memory)
        Database.Channel.create()
        Database.Channel.copying(node(), :disk)
        Database.ChannelProc.create()
        Database.ChannelProc.copying(node(), :memory)
        Database.Cursor.create()
        Database.Cursor.copying(node(), :disk)
        Database.UserChannel.create()
        Database.UserChannel.copying(node(), :disk)
        Database.History.create()
        Database.History.copying(node(), :disk)
        Database.HistoryIndex.create()
        Database.HistoryIndex.copying(node(), :disk)
      "NO" ->
        :ok
      node_name ->
        node = String.to_atom(node_name)
        join(node)
    end
  end

  def id do
    :erlang.phash2({node(), System.os_time()}, 0xffffffff)
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

defmodule Database.Message do
  defstruct [:from, :text, :ts]
end
