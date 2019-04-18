defmodule Iris.Command do
  defmodule CreateChannel do
    defstruct [:id, :name, :sender_id, :members, :first_message, :ts]
  end

  defmodule SendMessage do
    defstruct [:id, :sender_id, :channel_id, :body, :ts]
  end

  defmodule ReceiveMessage do
    defstruct [:id, :received_id, :channel_id, :message_id, :ts]
  end

  defmodule ReadChannel do
    defstruct [:id, :reader_id, :channel_id, :ts]
  end
end
