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
end
