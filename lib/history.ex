defmodule Iris.History do
  require Logger

  alias Database.History, as: DbHistory
  alias Database.HistoryIndex, as: DbIndex
  #alias Database.Message, as: DbMessage

  def append_to_history(channel_id, message) do
    case DbHistory.read!(channel_id) do
      nil ->
        now = System.system_time
        %DbHistory{channel_id: channel_id,
                   start_ts: now,
                   last_ts: now,
                   messages: [message],
                   index: []}
        |> DbHistory.write!
      [history] ->
        history
        |> append_to_index(message)
    end
  end

  def read_history(channel_id) do
    case DbHistory.read!(channel_id) do
      nil ->
        []
      [history] ->
        # TODO read the index, too
        history.messages
    end
  end

  defp append_to_index(%{messages: msgs} = history, message) when length(msgs) < 20 do
    %DbHistory{history | messages: msgs ++ [message]}
    |> DbHistory.write!
  end
  defp append_to_index(history, message) do
    index_id = length(history.index) + 1
    now = System.system_time
    %DbIndex{id: index_id,
             messages: history.messages ++ [message],
             start_ts: history.start_ts,
             last_ts: now}
    |> DbIndex.write!
    # check the db errors
    %DbHistory{history | last_ts: now,
                         messages: [],
                         index: history.index ++ [index_id]}
    |> DbHistory.write!
  end
end
