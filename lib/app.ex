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
    Supervisor.start_link(__MODULE__, [])
  end

  def init(_) do
    children = [
      worker(Iris.Hook, [])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
