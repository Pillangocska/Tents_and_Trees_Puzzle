defmodule FirstElixirTest do
  use ExUnit.Case
  doctest FirstElixir

  test "greets the world" do
    assert FirstElixir.hello() == :world
  end
end
