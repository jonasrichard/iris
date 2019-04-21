defmodule Iris.Event do
  defmodule ChannelCreated do
    defstruct [:id, :channel, :name, :owner, :members]
  end

  defmodule MessageSent do
    defstruct [:id, :sender, :channel, :body, :ts]
  end

  defmodule MessageReceived do
    defstruct [:id, :receiver, :channel, :ts]
  end

  defmodule ChannelRead do
    defstruct [:id, :reader, :channel, :ts]
  end

  defmodule InboxMessageArrived do
    defstruct [:recipient, :channel, :sender, :body, :message_ts]
  end
end
