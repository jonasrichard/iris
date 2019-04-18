defmodule Iris.CommandDispatcher do
  use GenServer

  def start_link(params \\ []) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def send(commands) when is_list(commands) do
    for command <- commands, do: send(command)
  end

  def send(command) do
    GenServer.call(__MODULE__, {:send, command})
  end

  @impl true
  def init(_) do
    {:ok,
     %{
       Iris.Command.CreateChannel => Iris.CommandHandler.CreateChannel,
       Iris.Command.SendMessage => Iris.CommandHandler.SendMessage
     }}
  end

  def handle_call({:send, command}, _from, state) do
    module = state[command.__struct__]
    apply(module, :handle, [command])
    {:reply, :ok, state}
  end

  # TODO a generic command handler needs to
  # - read the aggregate
  # - call a method in the aggregate
  # - result one or more events
  # - save them to event bus
  #
  # Event bus need to save the event but should react on them async.
end
