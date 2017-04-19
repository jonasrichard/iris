defmodule Iris.ChannelTest do
  use ExUnit.Case

  alias Iris.Messenger, as: M

  test "u1 create channel, u2 should see it" do
    {:ok, u1, _u1s} = M.open("u1", "u1")
    {:ok, u2, _u2s} = M.open("u2", "u2")

    M.send_msg(u1, %{type: "channel.create",
                     name: "first u1",
                     invitees: ["u2"]})

    {:ok, created} = M.recv_msg(u1)
    assert created["type"] == "channel.created"
    assert created["name"] == "first u1"
    assert created["channelId"] != nil
    assert Enum.all?(~w(u1 u2), &(&1 in created["members"]))

    {:ok, invited} = M.recv_msg(u2)
    assert invited["type"] == "channel.invited"
    assert invited["name"] == "first u1"
    assert invited["channelId"] != nil
    assert Enum.all?(~w(u1 u2), &(&1 in invited["members"]))
  end
end
