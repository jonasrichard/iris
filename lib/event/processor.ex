defmodule Iris.Event.Processor do
  use GenServer
  @behaviour Iris.Processor

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Process an event, which executes all the registered handlers on that event
  """
  @impl Iris.Processor
  def process(event) do
    GenServer.cast(Process.whereis(__MODULE__), {:process, event})
  end

  def register(callback) do
    GenServer.cast(Process.whereis(__MODULE__), {:register, callback})
  end

  @impl
  def init(opts) do
    IO.puts("opts #{inspect opts}")
    {:ok, %{callbacks: Keyword.get(opts, :callbacks, [])}}
  end

  @impl
  def handle_cast({:process, event}, state) do
    for m <- state[:callbacks], do: m.(event)
    Iris.Event.Queue.ack(event.id)
    {:noreply, state}
  end
  def handle_cast({:register, callback}, state) do
    callbacks = state[:callbacks]
    {:noreply, Map.put(state, :callbacks, callbacks ++ [callback])}
  end
end
