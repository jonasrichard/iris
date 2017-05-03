defmodule Iris.ChannelTest do
  use ExUnit.Case
  require Logger

  alias Iris.Messenger, as: M
  alias Iris.Tracer, as: Tracer

  setup do
    on_exit fn ->
      Tracer.stop()
    end
  end

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

    M.send_msg(u1, %{type: "channel.list"})
    {:ok, u1cs} = M.recv_msg(u1)
    assert u1cs["type"] == "channel.list"
    assert length(u1cs["channels"]) > 0
    Logger.info("Channel list #{inspect u1cs}")
    assert u1cs["channels"]
           |> Enum.filter(&(&1["name"] == "first u1"))
           |> Enum.all?(fn(channel) -> "u2" in channel["members"] end)
  end
end
