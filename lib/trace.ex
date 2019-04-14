defmodule Iris.Tracer do
  def start(module) when is_atom(module) do
    :dbg.tracer()
    add_module(module)
    trace()
  end

  def start(modules) when is_list(modules) do
    :dbg.tracer()
    modules |> Enum.each(&add_module(&1))
    trace()
  end

  def stop do
    :dbg.stop_clear()
  end

  defp add_module(module) do
    :dbg.tpl(module, [])
  end

  defp trace do
    :dbg.p(:all, :c)
  end
end
