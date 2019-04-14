defmodule Iris.EventDispatcher do
  use GenServer

  require Logger

  def start_link(params \\ []) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def send(event) do
    GenServer.cast(__MODULE__, {:dispatch, event})
  end

  def init(_) do
    {:ok,
     %{
       Iris.Event.ChannelCreated => [
         {Iris.Aggregate.Channel, :channel_id},
         Iris.Projection.Inbox
       ],
       Iris.Event.MessageSent => [
         {Iris.Aggregate.Channel, :channel_id}
       ],
       Iris.Event.InboxMessageArrived => [
         Iris.Projection.Inbox
       ]
     }}
  end

  def handle_cast({:dispatch, event}, state) do
    handlers = state[event.__struct__]
    Logger.info("Handling #{inspect event} with #{inspect handlers}")

    for handler <- handlers do
      case apply_handler(event, handler) do
        nil ->
          # no new event triggered
          :ok

        events when is_list(events) ->
          # in case of GenServer.call it would be deadlock
          for event <- events, do: Iris.EventDispatcher.send(event)

        event ->
          Iris.EventDispatcher.send(event)
      end
    end

    {:noreply, state}
  end

  defp apply_handler(event, {module, id_field}) do
    Logger.info("#{module} handles #{inspect event}") 
    aggregate = apply(module, :load, [Map.get(event, id_field)])
    apply(module, :apply, [aggregate, event])
  end
  defp apply_handler(event, module) do
    Logger.info("#{module} handles #{inspect event}") 
    apply(module, :apply, [event])
  end
end
