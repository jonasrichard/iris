defmodule Iris.Command do
  # TODO maybe the create channel has a first text message as well? and other parties?
  defmodule CreateChannel do
    defstruct [:id, :name, :sender_id]
  end

  defmodule SendMessage do
    defstruct [:id, :sender_id, :channel_id, :body, :created_ts]
  end
end
