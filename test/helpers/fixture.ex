defmodule Iris.Fixture do
  def message(opts \\ []) do
    id = Keyword.get(opts, :id, id())
    sender_id = Keyword.get(opts, :sender_id, num_id())
    channel_id = Keyword.get(opts, :channel_id, num_id())
    ts = Keyword.get(opts, :created_ts, :os.timestamp()) |> now_to_utc()

    %Iris.Model.Message{
      id: id,
      sender_id: sender_id,
      channel_id: channel_id,
      created_ts: ts,
      body: "A new message"
    }
  end

  def now_to_utc() do
    now_to_utc(:erlang.timestamp())
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

  defp pad(i) when i > 9 do
    Integer.to_string(i)
  end

  defp pad(i) do
    Integer.to_string(i) |> String.pad_leading(2, "0")
  end

  def id() do
    1..3
    |> Enum.map(fn _ ->
      :rand.uniform(100_000)
      |> Integer.to_string()
      |> String.pad_leading(5, "0")
    end)
    |> Enum.join("-")
  end

  def num_id() do
    :rand.uniform(100_00) |> Integer.to_string()
  end

  def retry(fun) do
    retry(fun, 30)
  end

  def retry(fun, 0) do
    raise "Timeout calling evaluating: #{inspect fun}"
  end
  def retry(fun, num) do
    case fun.() do
      nil ->
        Process.sleep(1000)
        retry(fun, num - 1)
      result ->
        result
    end
  end
end
