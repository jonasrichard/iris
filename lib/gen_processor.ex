defmodule Iris.Processor do
  @callback process(%Iris.Database.Event{}) :: term
end
