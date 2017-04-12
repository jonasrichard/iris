defmodule Iris.Message do

  def hello do
    %{type: "hello"}
  end

  def bye do
    %{type: "bye"}
  end

  def session(id) do
    %{type: "authenticated", sessionId: id}
  end

  def error(message) do
    %{type: "error", message: message}
  end
end
