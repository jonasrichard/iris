defmodule Iris.Message do

  def hello do
    %{type: "hello"}
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

  def parse(%{"type" => "channel.create"} = msg) do
    atomize(msg, [:type, :name, :channelId, :invitees])
  end
  def parse(%{"type" => "channel.list"}) do
    {:ok, %{type: :'channel.list'}}
  end
  def parse(%{"type" => "bye"}) do
    {:ok, %{type: :bye}}
  end
  def parse(%{"type" => "auth"} = msg) do
    atomize(msg, [:type, :user, :pass])
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
        Enum.reduce(optional, map,
            fn field, acc ->
              case msg[Atom.to_string(field)] do
                nil ->
                  acc
                value ->
                  Map.put(acc, field, value)
              end
            end)
    end
  end

  defp mandatories(msg, mandatory) do
    Enum.reduce_while(mandatory, %{},
        fn field, map ->
          case msg[Atom.to_string(field)] do
            nil ->
              {:halt, {:error, {"Missing mandatory field #{field}"}}}
            value ->
              {:cont, Map.put(map, field, value)}
          end
        end)
  end
end
