defmodule Iris.App do
  use Application

  def start(_type, _args) do
    Iris.MainSup.start_link()
  end
end

defmodule Iris.MainSup do
  use Supervisor

  import Supervisor.Spec

  def start_link do
    Supervisor.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def init(_) do
    children = [
      worker(Iris.Hook, []),
      worker(Iris.ClientSup, [])
    ]
    supervise(children, strategy: :one_for_one)
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

  def start_child do
    Supervisor.start_child(__MODULE__, [])
  end

  def init(_) do
    children = [
      worker(Iris.Channel, [])
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end
