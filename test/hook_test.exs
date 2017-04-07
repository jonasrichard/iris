defmodule Iris.HookTest do
  use ExUnit.Case

  test "add/delete hook works" do
    # :dbg.tracer
    # :dbg.tpl(Iris.Hook, [])
    # :dbg.p(:all, :c)

    :ok = Iris.Hook.add(:test_hook, __MODULE__, :first, 10)
    assert Iris.Hook.run(:test_hook, [0]) == [1]

    :ok = Iris.Hook.add(:test_hook, __MODULE__, :second, 20)
    assert Iris.Hook.run(:test_hook, [0]) == [3]

    :ok = Iris.Hook.delete(:test_hook, __MODULE__, :first, 10)
    assert Iris.Hook.run(:test_hook, [0]) == [2]
  end

  def first(n) do
    {:ok, [n + 1]}
  end

  def second(n) do
    {:ok, [n + 2]}
  end
end
