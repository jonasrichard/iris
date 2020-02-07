defmodule Iris.EventDispatcher do
  use GenServer

  require Logger

  def start_link(params \\ []) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def dispatch(events) when is_list(events) do
    for {partition, event} <- events, do: send(partition, event)
  end

  def dispatch(partition, event) do
    GenServer.cast(__MODULE__, {:dispatch, partition, event})
  end

  def init(_) do
    {:ok, nil}
  end

  def handle_cast({:dispatch, partition, event}, state) do
    Logger.info("Dispatch partition: #{partition} event: #{inspect(event)}")

    json = Iris.Util.struct_to_json(event)

    # TODO check the return value
    KafkaEx.produce("channel", partition, json)
    {:noreply, state}
  end
end
