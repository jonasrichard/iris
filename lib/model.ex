defmodule Iris.Model.Message do
  defstruct [:id, :sender_id, :channel_id, :body, :created_ts]
end

defmodule Iris.Model.Message.Stored do
  defstruct [:id, :recipient_id, :channel_id, :stored_ts]
end

defmodule Iris.Model.Message.Received do
  defstruct [:id, :sender_id, :channel_id, :message_id, :received_ts]
end

defmodule Iris.Model.Message.Read do
  defstruct [:id, :sender_id, :recipient_id, :channel_id, :message_id, :read_ts]
end

defmodule Iris.Model.Channel.Create do
  defstruct [:id, :sender_id, :name, :invited_ids]
end

defmodule Iris.Model.Channel.Created do
end

defmodule Iris.Model.Session.Create do
  defstruct [:sender_id, :password]
  # metadata can come here like ip address, etc
end

defmodule Iris.Model.Session.Created do
  defstruct [:id, :recipient_id]
  # features can come here
end
