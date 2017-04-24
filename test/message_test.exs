defmodule Iris.MessageTest do
  use ExUnit.Case

  alias Iris.Messenger, as: M

  setup_all do
    # Setup a channel with three users and two of them is online
    {:ok, u1, _} = M.open("u1", "u1")
    {:ok, u2, _} = M.open("u2", "u2")

    M.send_msg(u1, %{type: "channel.create",
                     name: "msg test",
                     invitees: ["u1", "u2", "u3"]})

    {:ok, created} = M.recv_msg(u1)
    {:ok, _invited} = M.recv_msg(u2)

    [u1: u1, u2: u2, channel: created["channelId"]]
  end

  test "u1 send message into channel", context do
    %{u1: u1, u2: u2, channel: channel} = context
    # TODO messenger shoud put from in the message
    M.send_msg(u1, %{type: "message",
                     subtype: "send",
                     channel: channel,
                     from: "u1",
                     ts: Iris.Message.ts(),
                     text: "Good morning, I am u1!"})

    {:ok, stored} = M.recv_msg(u1)
    assert stored["type"] == "message"
    assert stored["subtype"] == "stored"
    assert stored["channel"] == channel

    {:ok, incoming} = M.recv_msg(u2)
    assert incoming["type"] == "message"
    assert incoming["subtype"] == "incoming"
  end
end
