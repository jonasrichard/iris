defmodule Iris.Session do
  require Logger

  def save(pid, user) do
    Logger.debug fn -> "Session created" end
  end

  def delete(id) do
    Logger.debug fn -> "Session #{id} is removed" end
  end

  def find_by_id(id) do
    nil
  end

  def find_by_name(user) do
    nil
  end

  def send_message(user, message) do
    case find_by_name(user) do
      nil ->
        :offline
      [session] ->
        send session.pid, {:route, message}
      sessions ->
        sessions
        |> Enum.each(&(send &1.pid, {:route, message}))
    end
  end
end
