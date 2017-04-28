defmodule Iris.Session do
  require Logger
  alias Database.Session, as: S

  def save(pid, user) do
    # Kick out previous sessions
    with sessions when is_list(sessions) <- find_by_name(user),
         do: sessions
             |> Enum.each(fn(session) -> send session.pid, :kick_out end)
    # Save new session
    id = Database.id()
    Logger.debug("Session created with #{id} for user #{user}")
    %S{id: id, pid: pid, user: user} |> S.write!
  end

  def delete(id) do
    Logger.debug("Session #{id} is removed")
    S.delete!(id)
  end

  def find_by_id(id) do
    S.read!(id)
  end

  def find_by_name(user) do
    S.read_at!(user, :user)
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
