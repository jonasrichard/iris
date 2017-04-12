defmodule Iris.Session do
  require Logger
  alias Database.Session, as: S

  def save(pid, user) do
    id = id()
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

  defp id do
    :erlang.phash2({node(), System.os_time()}, 0xffffffff)
  end
end
