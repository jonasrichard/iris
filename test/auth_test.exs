defmodule Iris.AuthTest do
  use ExUnit.Case

  alias Iris.Messenger, as: M

  test "successful authentication" do
    {:ok, pid} = M.start_link
    hello = M.get(pid)
    assert hello["type"] == "hello"
  end
end
