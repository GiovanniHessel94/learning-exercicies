defmodule EnumStreamPractice do
  @moduledoc """
  Exercises from the `Chapter 3 - Control Flow` of the `Elixir In Action` book.
  """

  @path "./file.csv"
  @split_characters [" ", ",", "|"]

  @doc """
  Implementation of `large_lines!/1` function, given as an example of using `Stream` and `Enum` modules.
  """
  @spec large_lines!(String.t()) :: list(String.t())
  def large_lines!(path) do
    path
    |> File.stream!()
    |> Stream.map(&String.trim_trailing(&1, "\n"))
    |> Enum.filter(&String.length(&1) > 80)
  end

  @doc """
  Iterates over each line in the given path's file and returns a list of their lengths.
  """
  @spec lines_lengths!(String.t()) :: list(non_neg_integer())
  def lines_lengths!(path) do
    path
    |> File.stream!()
    |> Stream.map(&String.trim_trailing(&1, "\n"))
    |> Enum.map(&String.length(&1))
  end

  @doc """
  Iterates over each line in the given path's file and returns the length of the longest line.
  """
  @spec longest_line_length!(String.t()) :: non_neg_integer()
  def longest_line_length!(path) do
    path
    |> File.stream!()
    |> Stream.map(& &1 |> String.trim_trailing("\n") |> String.length())
    |> Enum.max()
  end

  @doc """
  Iterates over each line in the given path's file and returns the longest line.
  """
  @spec longest_line!(String.t()) :: String.t()
  def longest_line!(path) do
    path
    |> File.stream!()
    |> Stream.map(&String.trim_trailing(&1, "\n"))
    |> Enum.max_by(&String.length(&1))
  end

  @doc """
  Iterates over each line in the given path's file and returns the number of words per line.
  """
  @spec words_per_line!(String.t(), list(String.t())) :: list(non_neg_integer())
  def words_per_line!(path, split_characters \\ @split_characters) do
    path
    |> File.stream!()
    |> Stream.map(& &1 |> String.trim_trailing("\n") |> String.split(split_characters))
    |> Enum.map(&Enum.count(&1))
  end


  @doc """
  Returns the path to the `file.csv` file.
  """
  @spec path() :: String.t()
  def path, do: @path
end
