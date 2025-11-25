defmodule TodoServer do
  @moduledoc """
  Exercises code from the `Chapter 5 - Concurrency Primitives` of the `Elixir In Action` book.
  """

  @typedoc """
  The `TodoServer` supported message types.

  Unsupported messages are simply ignored.
  """
  @type message_t() ::
          {:add_entry, TodoList.entry_params_t()}
          | {:update_entry, TodoList.id_t(), (TodoList.entry_t() -> TodoList.entry_t())}
          | {:delete_entry, TodoList.id_t()}
          | {:entries, pid(), Date.t()}
          | term()

  ##################
  ##  Client API  ##
  ##################

  @doc """
  Returns the todo list server entries from a given date.

  Returns `{:error, :timeout}` if the entries are not received within 5 seconds.

  ## Parameters

  - `todo_server`: The todo list server pid to get the entries from.
  - `date`: The date to get the entries from.

  ## Examples

      iex> todo_server = TodoServer.start()
      #PID<0.123.0>

      iex> TodoServer.add_entry(todo_server, %{date: ~D[2025-12-22], title: "Shopping"})
      :ok

      iex> TodoServer.entries(todo_server, ~D[2025-12-22])
      [%{id: 1, date: ~D[2025-12-22], title: "Shopping"}]

      iex> TodoServer.entries(todo_server, ~D[2025-12-22])
      {:error, :timeout}

  """
  @spec entries(pid(), Date.t()) :: list(TodoList.entry_t()) | {:error, :timeout}
  def entries(todo_server, date) do
    send(todo_server, {:entries, self(), date})

    receive do
      {:entries, entries} -> entries
    after
      5000 -> {:error, :timeout}
    end
  end

  @doc """
  Adds a new entry to a todo list server.

  ## Parameters

  - `todo_server`: The todo list server pid to add the entry to.
  - `entry`: The entry to add.

  ## Examples

      iex> todo_server = TodoServer.start()
      #PID<0.123.0>

      iex> TodoServer.add_entry(todo_server, %{date: ~D[2025-12-20], title: "Dentist"})
      :ok

  """
  @spec add_entry(pid(), TodoList.entry_params_t()) :: :ok
  def add_entry(todo_server, entry) do
    send(todo_server, {:add_entry, entry})

    :ok
  end

  @doc """
  Updates an entry in a todo list server.

  ## Parameters

  - `todo_server`: The todo list server pid to update the entry in.
  - `entry_id`: The ID of the entry to update.
  - `updater_fun`: A function that updates the entry.

  ## Examples

      iex> todo_server = TodoServer.start()
      #PID<0.123.0>

      iex> TodoServer.add_entry(todo_server, %{date: ~D[2025-12-22], title: "Shopping"})
      :ok

      iex> TodoServer.update_entry(todo_server, 1, fn entry -> %{entry | date: ~D[2025-12-21]} end)
      :ok

  """
  @spec update_entry(pid(), TodoList.id_t(), (TodoList.entry_t() -> TodoList.entry_t())) :: :ok
  def update_entry(todo_server, entry_id, updater_fun) do
    send(todo_server, {:update_entry, entry_id, updater_fun})

    :ok
  end

  @doc """
  Deletes an entry from a todo list server.

  ## Parameters

  - `todo_server`: The todo list server pid to delete the entry from.
  - `entry_id`: The ID of the entry to delete.

  ## Examples

      iex> todo_server = TodoServer.start()
      #PID<0.123.0>

      iex> TodoServer.add_entry(todo_server, %{date: ~D[2025-12-22], title: "Shopping"})
      :ok

      iex> TodoServer.delete_entry(todo_server, 1)
      :ok

  """
  @spec delete_entry(pid(), TodoList.id_t()) :: :ok
  def delete_entry(todo_server, entry_id) do
    send(todo_server, {:delete_entry, entry_id})

    :ok
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts a new todo list server and returns the pid.
  """
  @spec start() :: pid()
  def start, do: spawn(fn -> loop(TodoList.new()) end)

  @spec loop(TodoList.t()) :: no_return()
  defp loop(todo_list) do
    todo_list =
      receive do
        message -> process_message(todo_list, message)
      end

    loop(todo_list)
  end

  @spec process_message(TodoList.t(), message_t()) :: TodoList.t()
  defp process_message(todo_list, {:add_entry, entry}) do
    TodoList.add_entry(todo_list, entry)
  end

  defp process_message(todo_list, {:update_entry, entry_id, updater_fun}) do
    TodoList.update_entry(todo_list, entry_id, updater_fun)
  end

  defp process_message(todo_list, {:delete_entry, entry_id}) do
    TodoList.delete_entry(todo_list, entry_id)
  end

  defp process_message(todo_list, {:entries, caller, date}) do
    send(caller, {:entries, TodoList.entries(todo_list, date)})

    todo_list
  end

  defp process_message(todo_list, _message) do
    todo_list
  end
end

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
  def new, do: %__MODULE__{}

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
  @spec update_entry(t(), id_t(), (entry_t() -> entry_t())) :: t()
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
  @spec delete_entry(t(), id_t()) :: t()
  def delete_entry(%__MODULE__{entries: entries} = todo_list, entry_id)
      when is_map_key(entries, entry_id),
      do: Map.update!(todo_list, :entries, &Map.delete(&1, entry_id))

  def delete_entry(%__MODULE__{} = todo_list, _entry_id), do: todo_list
end
