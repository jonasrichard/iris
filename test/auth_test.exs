defmodule Iris.AuthTest do
  use ExUnit.Case

  alias Iris.Messenger, as: M

  test "successful authentication" do
    {:ok, pid} = M.start_link()
    {:ok, hello} = M.recv_msg(pid)
    assert hello["type"] == "hello"

    M.send_msg(pid, %{type: "auth", user: "user1", pass: "user1"})
    {:ok, session} = M.recv_msg(pid)

    assert session["type"] == "authenticated"
    assert is_number(session["sessionId"])
  end

  test "unsuccessful authentication" do
    {:ok, pid} = M.start_link()
    {:ok, _hello} = M.recv_msg(pid)

    M.send_msg(pid, %{type: "auth", user: "user", pass: "bad"})
    {:ok, error} = M.recv_msg(pid)

    assert error["type"] == "error"
  end

  test "session created and cleared" do
    {:ok, pid} = M.start_link()
    {:ok, _hello} = M.recv_msg(pid)

    M.send_msg(pid, %{type: "auth", user: "user2", pass: "user2"})
    {:ok, session} = M.recv_msg(pid)

    sid = session["sessionId"]
    s = Iris.Session.find_by_id(sid)
    assert s.id == sid

    M.send_msg(pid, %{type: "bye"})
    {:ok, _bye} = M.recv_msg(pid)

    assert Iris.Session.find_by_id(sid) == :nil
  end
end
