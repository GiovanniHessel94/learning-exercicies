defmodule TodoList do
  @moduledoc """
  Exercises code from the `Chapter 4 - Data Abstraction` of the `Elixir In Action` book.
  """

  defstruct next_id: 1, entries: %{}

  @type id_t() :: non_neg_integer()
  @type entry_t() :: %{id: id_t(), date: Date.t(), title: String.t()}
  @type entry_params_t() :: %{date: Date.t(), title: String.t()}

  @typedoc """
  The `TodoList` struct type.

  Composed by:
  - `next_id`: The next ID to assign to a new entry.
  - `entries`: A map of entries where the key is the entry ID and the value is the data.

  """
  @type t() :: %__MODULE__{next_id: id_t(), entries: %{id_t() => entry_t()}}

  @doc """
  Creates a new empty todo list.

  ## Examples

  iex> TodoList.new()
  %TodoList{next_id: 1, entries: %{}}

  """
  @spec new() :: t()
  def new(), do: %__MODULE__{}

  @doc """
  Adds a new entry to a todo list.

  ## Parameters

  - `todo_list`: The todo list to add the entry to.
  - `entry`: The entry to add.

  ## Examples

  iex> todo_list = TodoList.new()
  %TodoList{next_id: 1, entries: %{}}

  iex> TodoList.add_entry(todo_list, %{date: ~D[2025-12-20], title: "Dentist"})
  %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}

  """
  @spec add_entry(t(), entry_params_t()) :: t()
  def add_entry(%__MODULE__{next_id: next_id, entries: entries} = todo_list, entry) do
    entry
    |> Map.put(:id, next_id)
    |> then(&Map.put(entries, &1.id, &1))
    |> then(&%__MODULE__{todo_list | entries: &1, next_id: next_id + 1})
  end

  @doc """
  Returns the todo list entries from a given date.

  ## Parameters

  - `todo_list`: The todo list to get the entries from.
  - `date`: The date to get the entries from.

  ## Examples

  iex> todo_list = %TodoList{next_id: 3, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}, 2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}
  %TodoList{next_id: 3, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}, 2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}

  iex> TodoList.entries(todo_list, ~D[2025-12-22])
  [%{id: 2, date: ~D[2025-12-22], title: "Shopping"}]

  """
  @spec entries(t(), Date.t()) :: list(entry_t())
  def entries(%__MODULE__{entries: entries} = _todo_list, %Date{} = date) do
    entries
    |> Map.values()
    |> Enum.filter(&(&1.date == date))
  end

  @doc """
  Updates an entry in a todo list.

  ## Parameters

  - `todo_list`: The todo list to update the entry in.
  - `entry_id`: The ID of the entry to update.
  - `updater_fun`: A function that updates the entry.

  ## Examples

  iex> todo_list = %TodoList{next_id: 3, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}, 2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}
  %TodoList{next_id: 3, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}, 2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}

  iex> TodoList.update_entry(todo_list, 1, fn entry -> %{entry | date: ~D[2025-12-21]} end)
  %TodoList{next_id: 3, entries: %{1 => %{id: 1, date: ~D[2025-12-21], title: "Dentist"}, 2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}

  """
  @spec update_entry(t(), non_neg_integer(), (entry_t() -> entry_t())) :: t()
  def update_entry(%__MODULE__{entries: entries} = todo_list, entry_id, updater_fun)
      when is_map_key(entries, entry_id),
      do: %__MODULE__{todo_list | entries: Map.update!(entries, entry_id, updater_fun)}

  def update_entry(%__MODULE__{} = todo_list, _entry_id, _updater_fun), do: todo_list

  @doc """
  Deletes an entry from a todo list.

  ## Parameters

  - `todo_list`: The todo list to delete the entry from.
  - `entry_id`: The ID of the entry to delete.

  ## Examples

  iex> todo_list = %TodoList{next_id: 3, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}, 2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}
  %TodoList{next_id: 3, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}, 2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}

  iex> TodoList.delete_entry(todo_list, 1)
  %TodoList{next_id: 3, entries: %{2 => %{id: 2, date: ~D[2025-12-22], title: "Shopping"}}}

  """
  @spec delete_entry(t(), non_neg_integer()) :: t()
  def delete_entry(%__MODULE__{entries: entries} = todo_list, entry_id)
      when is_map_key(entries, entry_id),
      do: Map.update!(todo_list, :entries, &Map.delete(&1, entry_id))

  def delete_entry(%__MODULE__{} = todo_list, _entry_id), do: todo_list
end

defimpl Collectable, for: TodoList do
  def into(term), do: {term, &into_callback/2}

  defp into_callback(todo_list, {:cont, entry}), do: TodoList.add_entry(todo_list, entry)
  defp into_callback(todo_list, :done), do: todo_list
  defp into_callback(_todo_list, :halt), do: :ok
end
