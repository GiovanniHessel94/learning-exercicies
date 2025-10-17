defmodule EnumStreamPracticeExample do
  @moduledoc """
  Example code from the `Chapter 3 - Control Flow` of the `Elixir In Action` book.
  """

  def large_lines!(path) do
    File.stream!(path)
    |> Stream.map(&String.trim_trailing(&1, "\n"))
    |> Enum.filter(&String.length(&1) > 80)
  end
end
