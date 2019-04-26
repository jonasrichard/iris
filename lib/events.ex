defmodule Iris.Event do
  defmodule ChannelCreated do
    defstruct [:id, :channel, :name, :owner, :members]
  end

  # members is event enrichment
  defmodule MessageSent do
    defstruct [:id, :sender, :channel, :body, :ts, :members]
  end

  defmodule MessageReceived do
    defstruct [:id, :receiver, :channel, :ts]
  end

  defmodule ChannelRead do
    defstruct [:id, :reader, :channel, :ts]
  end
end
