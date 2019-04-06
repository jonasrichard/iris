defmodule Iris.Event.Queue do
  use GenServer

  alias Iris.Database.Event, as: Event

  def start_link(_args \\ []) do
    # TODO move the processor callback from somewhere else
    Iris.Event.Processor.register(&Iris.Session.Processor.process/1)
    Iris.Event.Processor.register(&Iris.Channel.Processor.process/1) 
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def store(event_id, message) do
    GenServer.call(Process.whereis(__MODULE__), {:store, event_id, message})
  end

  def ack(event_id) do
    GenServer.call(Process.whereis(__MODULE__), {:ack, event_id})
  end

  @impl true
  def init(_) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:store, id, message}, _from, state) do
    %Event{id: id, ts: :os.timestamp(), message: message}
    |> Event.write! 
    |> Iris.Event.Processor.process
    {:reply, :ok, state}
  end
  def handle_call({:ack, id}, _from, state) do
    Event.delete!(id)
    {:reply, :ok, state}
  end
end
