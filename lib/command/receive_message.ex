defmodule Iris.Command.ReceiveMessage do
  defstruct [:id, :receiver, :channel, :message_id, :ts]
end
