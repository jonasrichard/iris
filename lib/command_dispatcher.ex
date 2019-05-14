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
    {:ok, conn} = Xandra.start_link(nodes: ["cassandra:9042"])
    # TODO put this into app.exs
    Process.put(:connection, conn)
    Iris.Database.create_namespace()
    Iris.Database.Channel.create_table()
    {:ok, :no_state}
  end

  @impl true
  def handle_call({:send, command}, _from, state) do
    module =
      command.__struct__
      |> Atom.to_string()
      |> String.replace(".Command.", ".CommandHandler.")
      |> String.to_atom()

    apply(module, :handle, [command])

    {:reply, :ok, state}
  end
end
