defmodule Iris.Message do

  def hello do
    %{"type" => "hello"}
  end

  def session(id) do
    %{"type" => "session",
      "sessionId" => id}
  end
end
