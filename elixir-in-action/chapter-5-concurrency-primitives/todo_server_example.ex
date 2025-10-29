defmodule TodoServerExample do
  @moduledoc """
  Example code from the `Chapter 5 - Concurrency Primitives` of the `Elixir In Action` book.
  """

  def start do
    spawn(fn -> loop(TodoListExample.new()) end)
  end

  def loop(todo_list) do
    new_todo_list =
      receive do
        message -> process_message(todo_list, message)
      end

    loop(new_todo_list)
  end

  def add_entry(todo_server, new_entry) do
    send(todo_server, {:add_entry, new_entry})
  end

  def entries(todo_server, date) do
    send(todo_server, {:entries, self(), date})

    receive do
      {:todo_entries, entries} -> entries
    after
      5000 -> {:error, :timeout}
    end
  end

  defp process_message(todo_list, {:add_entry, new_entry}) do
    TodoListExample.add_entry(todo_list, new_entry)
  end

  defp process_message(todo_list, {:entries, caller, date}) do
    send(caller, {:todo_entries, TodoListExample.entries(todo_list, date)})
    todo_list
  end
end

defmodule TodoListExample do
  @moduledoc """
  Example code from the `Chapter 4 - Data Abstraction` of the `Elixir In Action` book.
  """

  defstruct next_id: 1, entries: %{}

  def new(), do: %TodoListExample{}

  def add_entry(todo_list, entry) do
    entry = Map.put(entry, :id, todo_list.next_id)
    new_entries = Map.put(todo_list.entries, todo_list.next_id, entry)

    %TodoListExample{todo_list | entries: new_entries, next_id: todo_list.next_id + 1}
  end

  def entries(todo_list, date) do
    todo_list.entries
    |> Map.values()
    |> Enum.filter(fn entry -> entry.date == date end)
  end

  def update_entry(todo_list, entry_id, updater_fun) do
    case Map.fetch(todo_list.entries, entry_id) do
      :error ->
        todo_list

      {:ok, old_entry} ->
        new_entry = updater_fun.(old_entry)
        new_entries = Map.put(todo_list.entries, new_entry.id, new_entry)
        %TodoListExample{todo_list | entries: new_entries}
    end
  end
end

defimpl Collectable, for: TodoListExample do
  def into(original) do
    {original, &into_callback/2}
  end

  defp into_callback(todo_list, {:cont, entry}) do
    TodoListExample.add_entry(todo_list, entry)
  end

  defp into_callback(todo_list, :done), do: todo_list
  defp into_callback(_todo_list, :halt), do: :ok
end
