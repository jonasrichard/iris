defmodule Iris.Hook do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def add(hook, module, fun, prio) do
    GenServer.call(__MODULE__, {:add, hook, module, fun, prio})
  end

  def delete(hook, module, fun, prio) do
    GenServer.call(__MODULE__, {:delete, hook, module, fun, prio})
  end

  def run(hook, args) do
    case :ets.lookup(:hooks, hook) do
      [] ->
        :ok

      [{_, callbacks}] ->
        run_callbacks(callbacks, args)
    end
  end

  def init(_) do
    tab = :ets.new(:hooks, [:public, :named_table])
    {:ok, %{tab: tab}}
  end

  def handle_call({:add, hook, module, fun, prio}, _from, state) do
    case :ets.lookup(:hooks, hook) do
      [{^hook, callbacks}] ->
        new_cbs = sort_by_prio([{module, fun, prio} | callbacks])
        :ets.insert(:hooks, {hook, new_cbs})

      [] ->
        :ets.insert(:hooks, {hook, [{module, fun, prio}]})
    end

    {:reply, :ok, state}
  end

  def handle_call({:delete, hook, module, fun, prio}, _from, state) do
    case :ets.lookup(:hooks, hook) do
      [] ->
        :ok

      [{^hook, callbacks}] ->
        new_cbs = List.delete(callbacks, {module, fun, prio})
        :ets.insert(:hooks, {hook, new_cbs})
    end

    {:reply, :ok, state}
  end

  def handle_call(other, _from, state) do
    {:reply, {:error, {:invalid_call, other}}, state}
  end

  defp run_callbacks([], args) do
    args
  end

  defp run_callbacks([{module, fun, _prio} | rest], args) do
    try do
      apply(module, fun, args)
    catch
      ex, desc ->
        {:error, {ex, desc}}
    else
      :ok ->
        run_callbacks(rest, args)

      {:ok, new_args} ->
        run_callbacks(rest, new_args)

      :stop ->
        :ok

      {:stop, result} ->
        {:ok, result}

      {:error, _reason} = error ->
        error
    end
  end

  defp sort_by_prio(callbacks) do
    Enum.sort(callbacks, &(elem(&1, 2) <= elem(&2, 2)))
  end
end
