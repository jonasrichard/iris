defmodule Iris.AuthTest do
  use ExUnit.Case

  alias Iris.Messenger, as: M

  test "successful authentication" do
    {:ok, pid} = M.start_link
    {:ok, hello} = M.recv_msg(pid)
    assert hello["type"] == "hello"
    M.send_msg(pid, %{"type" => "auth",
                      "user" => "user1",
                      "pass" => "pass"})
    {:ok, session} = M.recv_msg(pid)
    assert session["type"] == "session"
    assert is_number(session["sessionId"])
  end
end
