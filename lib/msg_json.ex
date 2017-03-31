defmodule Iris.Message.Json do

  def parse(%{"type" => "message"} = json) do
    parse_message(json)
  end
  def parse(%{"type" => "channel.create"} = json) do
    json
  end

  defp parse_message(json) do
    json
  end
end
