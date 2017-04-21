defmodule Iris.ChannelTest do
  use ExUnit.Case

  alias Iris.Messenger, as: M
  alias Iris.Tracer, as: Tracer

  setup do
    on_exit fn ->
      Tracer.stop()
    end
  end

  test "u1 create channel, u2 should see it" do
    Tracer.start([Iris.Client, Iris.Messenger])
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

    M.send_msg(u1, %{type: "channel.list"})
    {:ok, u1cs} = M.recv_msg(u1)
    assert u1cs["type"] == "channel.list"
    assert length(u1cs["channels"]) == 1

    c1 = hd(u1cs["channels"])
    assert c1["name"] == "first u1"
    assert "u1" in c1["members"]
    assert "u2" in c1["members"]
    assert "u1" == c1["owner"]
  end
end
