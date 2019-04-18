defmodule Iris.Event do
  defmodule ChannelCreated do
    defstruct [:id, :channel_id, :name, :owner, :members]
  end

  defmodule MessageSent do
    defstruct [:id, :sender, :channel, :body, :message_ts]
  end

  defmodule InboxMessageArrived do
    defstruct [:user_id, :channel_id, :sender_id, :body, :message_ts]
  end
end
