defmodule Iris.AuthTest do
  use ExUnit.Case

  alias Iris.Messenger, as: M

  test "successful authentication" do
    {:ok, pid} = M.start_link
    hello = M.recv(pid)
    assert hello["type"] == "hello"
    M.send(pid, %{"type" => "auth",
                  "user" => "user1",
                  "pass" => "pass"})
    IO.inspect(M.recv(pid))
  end
end
