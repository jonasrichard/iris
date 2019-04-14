defmodule Iris.App do
  use Application
  require Logger

  def start(_type, _args) do
    # import Supervisor.Spec # , warn: false
    # TODO need to start session which register to processor
    Iris.Database.init()
    #Rexbug.start("Iris.Projection.Inbox")

    children = [
      Iris.EventDispatcher,
      Iris.CommandDispatcher,
      # Iris.Event.Processor,
      # Iris.Event.Queue,
      {DynamicSupervisor, strategy: :one_for_one, name: Iris.Session.Supervisor},
      # Iris.Metrics,
      # Iris.Metrics.Reporter,
      Plug.Cowboy.child_spec(
        scheme: :http,
        plug: Iris.Router,
        options: [dispatch: dispatch()]
      )
    ]

    opts = [strategy: :one_for_one, name: Iris.Main.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp dispatch() do
    [
      {:_,
       [
         {"/chat/[...]", Iris.Receiver.Websocket, []},
         {:_, Plug.Cowboy.Handler, {Iris.Router, []}}
       ]}
    ]
  end
end

defmodule Iris.Router do
  use Plug.Router

  plug(Plug.Static, at: "/dashboard", from: "priv/html")
  plug(:match)
  plug(:dispatch)

  match _ do
    send_resp(conn, 200, "Default answer")
  end
end
