defmodule Iris.Event do
  defmodule ChannelCreated do
    defstruct [:id, :channel_id, :name, :sender_id]
  end

  defmodule MessageSent do
    defstruct [:id, :sender_id, :channel_id, :body, :created_ts]
  end

  defmodule InboxMessageArrived do
    defstruct [:user_id, :channel_id, :sender_id, :body, :message_ts]
  end
end
