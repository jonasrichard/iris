defmodule Iris.App do
  use Application
  require Logger

  def start(_type, _args) do
    #import Supervisor.Spec # , warn: false
    Iris.Database.init()
    children = [
      Iris.Event.Processor,
      Iris.Event.Queue,
      #Iris.Metrics,
      #Iris.Metrics.Reporter,
      Plug.Cowboy.child_spec(scheme: :http, plug: Iris.Router)
    ]
    #
    opts = [strategy: :one_for_one, name: Iris.Main.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule Iris.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  match _ do
    send_resp(conn, 200, "Default answer")
  end
end
