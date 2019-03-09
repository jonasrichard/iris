defmodule Iris.Session.Connection do
  use GenServer

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def new_connection(user_id) do
    DynamicSupervisor.start_child(Iris.Session.Supervisor, {__MODULE__, [user_id: user_id]})
  end

  def init(opts) do
    user_id = Keyword.get(opts, :user_id)
    Logger.info("New connection user_id #{user_id}")
    {:ok, %{:user_id => user_id}}
  end
end
