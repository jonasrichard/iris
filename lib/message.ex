defmodule Iris.Message do

  def hello do
    %{type: "hello"}
  end

  def auth(name, pass) do
    %{type: "auth", user: name, pass: pass}
  end

  def bye do
    %{type: "bye"}
  end

  def session(id) do
    %{type: "authenticated", sessionId: id}
  end

  def error(message) do
    %{type: "error", message: message}
  end

  def channel_created(channel) do
    %{type: "channel.created",
      name: channel.name,
      channelId: channel.id,
      members: channel.members}
  end

  def channel_invited(channel) do
    %{type: "channel.invited",
      name: channel.name,
      channelId: channel.id,
      members: channel.members}
  end

  def channel(channel) do
    %{id: channel.id,
      name: channel.name,
      members: channel.members,
      created_ts: channel.created_ts,
      last_ts: channel.last_ts}
  end

  def channel_history(channel_id, messages) do
    %{type: "channel.history",
      channel: channel_id,
      messages: messages}
  end

  def channel_list(channels) do
    %{type: "channel.list",
      channels: channels}
  end

  def message_send(channel_id, from, text) do
    channel_id
    |> generic_message(from, ts(), text)
    |> Map.put(:subtype, "send")
  end

  def message_stored(channel_id, ts) do
    %{type: "message",
      subtype: "stored",
      channel: channel_id,
      ts: ts}
  end

  def message_incoming(channel_id, from, text) do
    channel_id
    |> generic_message(from, ts(), text)
    |> Map.put(:subtype, "incoming")
  end

  def message_received(channel_id, from, to, ts) do
    channel_id
    |> generic_message(from, ts)
    |> Map.put(:subtype, "received")
    |> Map.put(:to, to)
  end

  def message_read(channel_id, from, to, ts) do
    channel_id
    |> generic_message(from, ts)
    |> Map.put(:subtype, "read")
    |> Map.put(:to, to)
  end

  defp generic_message(channel_id, from, ts) do
    %{type: "message",
      channel: channel_id,
      from: from,
      ts: ts}
  end

  defp generic_message(channel_id, from, ts, text) do
    %{type: "message",
      channel: channel_id,
      from: from,
      ts: ts,
      text: text}
  end

  def message_archive(db_message) do
    %{type: "message",
      subtype: "archive",
      from: db_message.from,
      ts: db_message.ts,
      text: db_message.text}
  end

  def parse(%{"type" => "message"} = msg) do
    case msg["subtype"] do
      "send" ->
        atomize(msg, [:type, :subtype, :channel, :from, :text])
      "received" ->
        atomize(msg, [:type, :subtype, :channel, :from, :to, :ts])
      "read" ->
        atomize(msg, [:type, :subtype, :channel, :from, :to, :ts])
    end
  end
  def parse(%{"type" => "channel.history"} = msg) do
    atomize(msg, [:type, :channel])
  end
  def parse(%{"type" => "channel.status"} = msg) do
    atomize(msg, [:type, :channel])
  end
  def parse(%{"type" => "channel.list"}) do
    {:ok, %{type: "channel.list"}}
  end
  def parse(%{"type" => "channel.create"} = msg) do
    atomize(msg, [:type, :name, :invitees])
  end
  def parse(%{"type" => "bye"}) do
    {:ok, %{type: "bye"}}
  end
  def parse(%{"type" => "auth"} = msg) do
    atomize(msg, [:type, :user, :pass])
  end

  @doc "Generate an id from os timestamp"
  def ts do
    {mega, sec, micro} = :os.timestamp()
    t1 = Integer.to_string(mega * 1_000_000 + sec)
    t2 = Integer.to_string(micro)
    t1 <> "." <> String.pad_leading(t2, 6, "0")
  end

  defp atomize(msg, mandatory, optional \\ []) do
    case atomize2(msg, mandatory, optional) do
      {:error, _} = error ->
        error
      map ->
        {:ok, map}
    end
  end

  defp atomize2(msg, mandatory, optional) do
    case mandatories(msg, mandatory) do
      {:error, _} = error ->
        error
      map ->
        Enum.reduce(optional, map, fn(field, map) -> atomize_field(msg, map, field) end)
    end
  end

  defp atomize_field(msg, map, field) do
    case msg[Atom.to_string(field)] do
      nil ->
        map
      value ->
        Map.put(map, field, value)
    end
  end

  defp mandatories(msg, mandatory) do
    Enum.reduce_while(mandatory, %{},
        fn(field, map) ->
          case msg[Atom.to_string(field)] do
            nil ->
              {:halt, {:error, {"Missing mandatory field #{field}"}}}
            value ->
              {:cont, Map.put(map, field, value)}
          end
        end)
  end
end
