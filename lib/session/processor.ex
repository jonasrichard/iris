defmodule Iris.Session.Processor do
  @behaviour Iris.Processor

  def process(event) do
    case event.message.__struct__ do
      Iris.Model.Session.Create ->
        Iris.Session.Connection.new_connection(event.message.sender_id)
      _ ->
        :ok
    end
  end
end
