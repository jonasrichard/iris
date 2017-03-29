defmodule Iris.Hook do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [])
  end

  def init(_) do
    tab = :ets.new(:hooks, [:public, :named_table])
    {:init, %{tab: tab}}
  end

  def handle_call({:add, hook, module, fun, prio}, _from, state) do
    case :ets.lookup(:hooks, hook) do
      [{^hook, callbacks}] ->
        new_hook = sort_by_prio([{module, fun, prio} | callbacks])
        :ets.insert(:hooks, new_hook)
      [] ->
        :ets.insert(:hooks, {hook, [{module, fun, prio}]})
    end
    {:reply, :ok, state}
  end

  defp sort_by_prio(callbacks) do
    Enum.sort(callbacks, &(elem(&1, 2) <= elem(&2, 2)))
  end
end
