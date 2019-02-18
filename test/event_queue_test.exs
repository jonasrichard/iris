defmodule Iris.Event.QueueTest do
  use ExUnit.Case

  test "store a text message" do
    message = Iris.Fixture.message()
    IO.inspect(message)
  end
end
