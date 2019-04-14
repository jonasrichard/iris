defmodule Iris.Receiver.DecoderTest do
  use ExUnit.Case

  doctest Iris.Receiver.Decoder

  alias Iris.Receiver.Decoder, as: Decoder

  test "decode text message" do
    json = """
    {
      "type": "message",
      "id": "123-567",
      "sender_id": "9",
      "channel_id": "28",
      "body": "This is the text really",
      "created_ts": "2016-09-04T14:52:23.976"
    }
    """

    assert {:ok, decoded} = Decoder.decode(json)
    assert decoded.id == "123-567"
    assert decoded.sender_id == "9"
    assert decoded.channel_id == "28"
    assert decoded.body == "This is the text really"
    assert decoded.created_ts == "2016-09-04T14:52:23.976"
  end
end
