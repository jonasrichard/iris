defmodule Iris.CommandDispatcher do
  use GenServer

  require Logger

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

    apply(module, :handle, [command])

    {:reply, :ok, state}
  end
end
