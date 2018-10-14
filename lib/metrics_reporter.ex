defmodule Iris.Metrics.Reporter do

  alias Iris.Metrics.Erlang, as: Erlang

  def child_spec(_args) do
    %{id: __MODULE__,
      start: {__MODULE__, :start_link, []},
      restart: :permanent,
      shutdown: 5000,
      type: :worker
    }
  end

  def start_link() do
    Task.start_link(fn -> loop() end)
  end

  def loop() do
    report()
    Process.sleep(1000)
    loop()
  end

  defp report() do
    memory = :erlang.memory()
    data = %Erlang{}
    data = %{data |
      fields: %{data.fields |
        total: Keyword.get(memory, :total),
        processes: Keyword.get(memory, :processes)}}
    data = %{data | tags: %{data.tags | server: "iris"}}
    Iris.Metrics.write(data)
  end
end
