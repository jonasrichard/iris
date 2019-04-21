defmodule Iris.EventDispatcher do
  use GenServer

  require Logger

  def start_link(params \\ []) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def send(events) when is_list(events) do
    for event <- events, do: send(event)
  end

  def send(event) do
    GenServer.cast(__MODULE__, {:dispatch, event})
  end

  def init(_) do
    {:ok,
     %{
       Iris.Event.ChannelCreated => [Iris.Projection.Inbox],
       Iris.Event.MessageSent => [Iris.Projection.Inbox],
     }
    }
  end

  def handle_cast({:dispatch, event}, state) do
    case state[event.__struct__] do
      nil ->
        :ok

      handlers ->
        Logger.info("Handling #{inspect event} with #{inspect handlers}")

        for handler <- handlers do
          apply(handler, :apply, [event])
        end
    end

    {:noreply, state}
  end
end
