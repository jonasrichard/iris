defmodule Iris.Event.QueueTest do
  use ExUnit.Case

  setup do
    :ok
  end

  test "store a text message" do
    test_process = self()
    f = fn evt ->
      IO.puts("Event processor callback #{inspect evt}")
      send(test_process, :ready)
    end

    Iris.Event.Processor.register(f)
    message = Iris.Fixture.message()
    IO.inspect(message)
    Iris.Event.Queue.store("1", message)
    receive do
      :ready ->
        :ok
    after 5000 ->
        flunk("Processor hasn't been invoked")
    end
  end

  test "remove acked message" do
    message = Iris.Fixture.message()
    Iris.Event.Queue.store("2", message)
    Iris.Event.Queue.ack("2")
    assert Iris.Database.Event.read!("2") == nil
  end
end
