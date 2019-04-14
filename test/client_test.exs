defmodule Iris.ClientTest do
  use ExUnit.Case

  alias Iris.Client, as: Client
  alias Iris.Message, as: Message
  alias Iris.ProcessMock, as: ProcessMock

  test "routed messages sent to web socket" do
    {:ok, w1} = ProcessMock.start_link()
    {:ok, c1} = Client.start_link(w1)

    send(c1, {:route, Message.hello()})
    assert_receive %{"type" => "hello"}
  end
end

defmodule Iris.ProcessMock do
  use GenServer
  require Logger

  def start_link do
    GenServer.start_link(__MODULE__, [self()], [])
  end

  def init([pid]) do
    {:ok, %{pid: pid}}
  end

  def handle_info(msg, state) do
    Logger.debug("Info #{inspect(msg)}")

    case msg do
      {:text, text} ->
        {:ok, json} = Poison.decode(text)
        send(state[:pid], json)

      _ ->
        :ok
    end

    {:noreply, state}
  end
end
