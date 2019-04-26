defmodule Iris.Event do
  defmodule ChannelCreated do
    defstruct [:id, :channel, :name, :owner, :members]
  end

  defmodule UserInvited do
    defstruct [:id, :channel, :inviter, :invitee, :ts, :members]
  end

  defmodule MemberKickedOut do
    defstruct [:id, :channel, :kicker, :kickee, :ts, :members]
  end

  # members is event enrichment
  defmodule MessageSent do
    defstruct [:id, :message_id, :sender, :channel, :body, :ts, :members]
  end

  defmodule MessageReceived do
    defstruct [:id, :receiver, :channel, :received_message_id, :ts]
  end

  defmodule ChannelRead do
    defstruct [:id, :reader, :channel, :ts]
  end
end
