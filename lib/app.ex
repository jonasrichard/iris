defmodule Iris.App do
  use Application
  require Logger

  def start(_type, _args) do
    import Supervisor.Spec

    case Application.fetch_env!(:iris, :database_type) do
      :mnesia ->
        Iris.Mnesia.init()
      _ ->
        nil
    end

    #Rexbug.start("KafkaEx.NetworkClient.create_socket/4 :: return;stack")
    Rexbug.start("KafkaEx.ConsumerGroup.init/1 :: stack")
    #RexBug.start("KafkaEx.Server0P10AndLater.init/1")

    children =
      case Application.fetch_env!(:iris, :database_type) do
        :cassandra ->
          [Iris.Cassandra]
        _ ->
          []
      end ++ [
      Iris.EventDispatcher,
      Iris.CommandDispatcher,
      {DynamicSupervisor, strategy: :one_for_one, name: Iris.Session.Supervisor},
      supervisor(
        KafkaEx.ConsumerGroup,
        [Iris.Consumer.Channel, "channel-group", ["channel"], []]
      ),
      Iris.Metrics,
      Iris.Metrics.Reporter,
      Plug.Cowboy.child_spec(
        scheme: :http,
        plug: Iris.Router,
        options: [
          dispatch: dispatch(),
          transport_options: [num_acceptors: 1]
        ]
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
