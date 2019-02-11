defmodule Iris.Model.Message do
  defstruct [
    :id,
    :sender,
    :channel_id,
    :body,
    :created_ts
  ]
end

defmodule Iris.Model.Message.Stored do
  defstruct [
    :id,
    :recipient,
    :channel_id,
    :stored_ts
  ]
end

defmodule Iris.Model.Message.Received do
  defstruct [
    :id,
    :sender,
    :channel_id,
    :message_id,
    :received_ts
  ]
end

defmodule Iris.Model.Message.Read do
  defstruct [
    :id,
    :sender,
    :recipient,
    :channel_id,
    :message_id,
    :read_ts
end

defmodule Iris.Model.Channel.Create do
  defstruct [
    :id,
    :sender,
    :name,
    :invite  # list of user ids to invite
  ]
end

defmodule Iris.Model.Channel.Created do
end

defmodule Iris.Model.Session.Create do
  defstruct [
    :sender,
    :password
    # metadata can come here like ip address, etc
  ]
end

defmodule Iris.Model.Session.Created do
  defstruct [
    :id,
    :recipient
    # features can come here
  ]
end
