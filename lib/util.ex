defmodule Iris.Util do
  def now_to_utc() do
    now_to_utc(:os.timestamp())
  end

  def now_to_utc(now) do
    {{year, month, day}, {hour, minute, second}} = :calendar.now_to_datetime(now)

    [
      pad(year),
      "-",
      pad(month),
      "-",
      pad(day),
      "T",
      pad(hour),
      ":",
      pad(minute),
      ":",
      pad(second)
    ]
    |> Enum.join()
  end

  def uuid_to_partition(uuid) do
    uuid |> String.slice(0, 8) |> String.to_integer(16) |> rem(50)
  end

  defp pad(i) when i > 9 do
    Integer.to_string(i)
  end

  defp pad(i) do
    Integer.to_string(i) |> String.pad_leading(2, "0")
  end
end
