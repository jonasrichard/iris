defmodule Iris.CommandDispatcher do
  use GenServer

  require Logger

  # TODO make versioned change type

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
    # TODO move this to the app start part
    Iris.Database.init()
    {:ok, :no_state}
  end

  @impl true
  def handle_call({:send, command}, _from, state) do
    module =
      command.__struct__
      |> Atom.to_string()
      |> String.replace(".Command.", ".CommandHandler.")
      |> String.to_atom()

    Logger.info("Calling #{module} :handle with #{inspect(command)}")

    case apply(module, :handle, [command]) do
      {:ok, {type, id, changes}} ->
        # append event to event store
        process_changes(type, id, changes)
        _partition = Iris.Util.uuid_to_partition(id)
        for {event, _} <- changes do
          Iris.EventDispatcher.dispatch(0, event)
        end
      {:error, reason} ->
        # command has been rejected
        # TODO different gen_server reply
        nil
        Logger.error("#{inspect(reason)}")
    end

    {:reply, :ok, state}
  end

  defp process_changes(:channel, id, changes) do
    for {event, version} <- changes do
      Iris.Database.append_changes(id, event, version)
    end
  end
end
