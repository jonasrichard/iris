defmodule Iris.App do
  use Application
  require Logger

  def start(_type, _args) do
    Iris.MainSup.start_link()
  end
end

defmodule Iris.MainSup do
  use Supervisor
  require Logger

  def start_link do
    Supervisor.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def init(_) do
    children = [
      Iris.Metrics,
      Iris.Metrics.Reporter
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end
