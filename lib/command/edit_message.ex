defmodule Iris.Command.EditMessage do
  defstruct [:id, :user, :channel, :message_id, :new_body, :ts]
end
