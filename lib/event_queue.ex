defmodule Iris.Event.Queue do
  use GenServer

  alias Iris.Database.Event, as: Event

  def start_link(_args \\ []) do
    GenServer.start_link(__MODULE__, [])
  end

  def store(id, message) do
    GenServer.call(Process.whereis(__MODULE__), {:store, id, message})
  end

  def init(_) do
    {:ok, %{}}
  end

  def handle_call({:store, id, message}, _from, state) do
    %Event{id: id, ts: :os.timestamp(), message: message}
    |> Event.write 
    {:reply, :ok, state}
  end
end
