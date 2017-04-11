defmodule Iris.Session do
  alias Database.Session, as: S

  def save(pid, user) do
    %S{id: id(), pid: pid, user: user} |> S.write!
  end

  def delete(_id) do
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
