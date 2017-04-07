defmodule Iris.Tracer do
  defmacro __using__(opts) do
    quote do
      def _trace do
        :dbg.tracer
        :dbg.tpl(__MODULE__, [])
        :dbg.p(:all, :c)
      end
    end
  end
end
