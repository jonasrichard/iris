defmodule Iris.Session do
  require Logger
  alias Database.Session, as: S

  def save(pid, user) do
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

  def find_by_name(_user) do
  end
end
