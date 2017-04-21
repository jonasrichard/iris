defmodule Iris.App do
  use Application
  require Logger

  def start(_type, _args) do
    Logger.info("Initializing database")
    Database.init()
    Iris.MainSup.start_link()
  end
end

defmodule Iris.MainSup do
  use Supervisor
  require Logger

  import Supervisor.Spec

  def start_link do
    Supervisor.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def init(_) do
    children = [
      worker(Iris.Hook, []),
      supervisor(Iris.ChannelSup, []),
      supervisor(Iris.ClientSup, [])
    ]
    result = supervise(children, strategy: :one_for_one)
    start_cowboy()
    result
  end

  defp start_cowboy do
    dispatch = :cowboy_router.compile([
                {:'_', [
                    {'/api/[...]', Iris.ApiHandler, []},
                    {'/ws', Iris.WSHandler, []},
                    {'/[...]', :cowboy_static, {:priv_dir, :iris, 'html'}}
                  ]}
                ])
    Logger.info("Starting cowboy")
    :cowboy.start_clear(:iris_http_listener, 5,
                        [port: 8080],
                        %{env: %{dispatch: dispatch}})
  end
end

defmodule Iris.ClientSup do
  use Supervisor

  import Supervisor.Spec

  def start_link do
    Supervisor.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def start_child(ws_pid) do
    Supervisor.start_child(__MODULE__, [ws_pid])
  end

  def init(_) do
    children = [
      worker(Iris.Client, [])
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end

defmodule Iris.ChannelSup do
  use Supervisor

  import Supervisor.Spec

  def start_link do
    Supervisor.start_link(__MODULE__, [], [name: __MODULE__])
  end

  @doc "Start a channel process without global registration"
  def start_child(channel) do
    Supervisor.start_child(__MODULE__, [channel])
  end

  def init(_) do
    children = [
      worker(Iris.Channel, [])
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end
