defmodule ServerProcess do
  @moduledoc """
  Exercises code from the `Chapter 6 - Generic Server Processes` of the `Elixir In Action` book.
  """

  @doc """
  Sends a call message to a server process and returns the response.

  Returns `{:error, :timeout}` if the response is not received within 5 seconds.

  ## Parameters

  - `server_pid`: The server process pid to send the call message to.
  - `message`: The message to send to the server process.

  ## Examples

      iex> todo_server = ServerProcess.start(TodoServer)
      #PID<0.123.0>

      iex> ServerProcess.cast(todo_server, {:add_entry, %{date: ~D[2025-12-20], title: "Dentist"}})
      :ok

      iex> ServerProcess.call(todo_server, {:entries, ~D[2025-12-20]})
      [%{id: 1, date: ~D[2025-12-20], title: "Dentist"}]

      iex> ServerProcess.call(todo_server, {:entries, ~D[2025-12-20]})
      {:error, :timeout}

  """
  @spec call(pid(), message :: term()) :: response :: term() | {:error, :timeout}
  def call(server_pid, message) do
    send(server_pid, {:call, message, self()})

    receive do
      {:response, response} -> response
    after
      5000 -> {:error, :timeout}
    end
  end

  @doc """
  Sends a cast message to a server process.

  ## Parameters

  - `server_pid`: The server process pid to send the cast message to.
  - `message`: The message to send to the server process.

  ## Examples

      iex> todo_server = ServerProcess.start(TodoServer)
      #PID<0.123.0>

      iex> ServerProcess.cast(todo_server, {:add_entry, %{date: ~D[2025-12-20], title: "Dentist"}})
      :ok

  """
  @spec cast(pid(), message :: term()) :: :ok
  def cast(server_pid, message) do
    send(server_pid, {:cast, message})

    :ok
  end

  @doc """
  Starts a new server process and returns the pid.

  Receives a callback module which must implement the `init/0`, `handle_call/2` and `handle_cast/2` callbacks.
  """
  @spec start(module()) :: pid()
  def start(callback_module) do
    spawn(fn -> loop(callback_module, callback_module.init()) end)
  end

  @spec loop(module(), state :: term()) :: no_return()
  defp loop(callback_module, state) do
    state =
      receive do
        message -> process_message(callback_module, state, message)
      end

    loop(callback_module, state)
  end

  @spec process_message(
          module(),
          state :: term(),
          message :: term()
        ) :: state :: term()
  defp process_message(callback_module, state, {:call, message, caller}) do
    message
    |> callback_module.handle_call(state)
    |> tap(fn {response, _state} -> send(caller, {:response, response}) end)
    |> then(fn {_response, state} -> state end)
  end

  defp process_message(callback_module, state, {:cast, message}) do
    callback_module.handle_cast(message, state)
  end

  defp process_message(_callback_module, state, _message) do
    state
  end
end

defmodule TodoServer do
  @moduledoc """
  Exercises code from the `Chapter 6 - Generic Server Processes` of the `Elixir In Action` book.

  This implementation is powered by the `ServerProcess` module.
  """

  @typedoc """
  The `TodoServer` supported call message types.

  Unsupported messages returns `{:error, :unsupported_message}` as response.
  """
  @type call_message_t() :: {:entries, Date.t()} | term()

  @typedoc """
  The `TodoServer` supported cast message types.

  Unsupported messages are simply ignored.
  """
  @type cast_message_t() ::
          {:add_entry, TodoList.entry_params_t()}
          | {:update_entry, TodoList.id_t(), (TodoList.entry_t() -> TodoList.entry_t())}
          | {:delete_entry, TodoList.id_t()}
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
  def entries(todo_server, date), do: ServerProcess.call(todo_server, {:entries, date})

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
  def add_entry(todo_server, entry), do: ServerProcess.cast(todo_server, {:add_entry, entry})

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
    ServerProcess.cast(todo_server, {:update_entry, entry_id, updater_fun})
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
    ServerProcess.cast(todo_server, {:delete_entry, entry_id})
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts a new todo list server and returns the pid.
  """
  @spec start() :: pid()
  def start, do: ServerProcess.start(TodoServer)

  @doc """
  Initializes the todo list server state as an empty todo list.
  """
  @spec init() :: TodoList.t()
  def init(), do: TodoList.new()

  @doc """
  Handles a call message.

  Returns a `{response, state}` tuple to match the `ServerProcess` contract.

  ## Parameters

  - `message`: The call message to handle. Returns response as `{:error, :unsupported_message}` when message is unsupported.
  - `todo_list`: The todo list state from the server.

  ## Examples

      iex> todo_list = TodoServer.init()
      %TodoList{next_id: 1, entries: %{}}

      iex> todo_list = TodoServer.handle_cast(todo_list, {:add_entry, %{date: ~D[2025-12-20], title: "Dentist"}})
      %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}

      iex> TodoServer.handle_call(todo_list, {:entries, ~D[2025-12-20]})
      {[%{id: 1, date: ~D[2025-12-20], title: "Dentist"}], %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}}

      iex> TodoServer.handle_call(todo_list, {:unsupported_message, self()})
      {{:error, :unsupported_message}, %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}}

  """
  @spec handle_call(
          call_message_t(),
          TodoList.t()
        ) :: {response :: list(TodoList.entry_t()) | {:error, :unsupported_message}, TodoList.t()}
  def handle_call({:entries, date}, todo_list), do: {TodoList.entries(todo_list, date), todo_list}
  def handle_call(_message, todo_list), do: {{:error, :unsupported_message}, todo_list}

  @doc """
  Handles a cast message.

  Returns the state to match the `ServerProcess` contract.

  ## Parameters

  - `message`: The cast message to handle.
  - `todo_list`: The todo list state from the server.

  ## Examples

      iex> todo_list = TodoServer.init()
      %TodoList{next_id: 1, entries: %{}}

      iex> todo_list = TodoServer.handle_cast(todo_list, {:add_entry, %{date: ~D[2025-12-20], title: "Dentist"}})
      %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}

      iex> todo_list = TodoServer.handle_cast(todo_list, {:delete_entry, 1})
      %TodoList{next_id: 2, entries: %{}}

      iex> TodoServer.handle_cast(todo_list, {:unsupported_message, 1})
      %TodoList{next_id: 2, entries: %{}}

  """
  @spec handle_cast(cast_message_t(), TodoList.t()) :: TodoList.t()
  def handle_cast({:add_entry, entry}, todo_list) do
    TodoList.add_entry(todo_list, entry)
  end

  def handle_cast({:update_entry, entry_id, updater_fun}, todo_list) do
    TodoList.update_entry(todo_list, entry_id, updater_fun)
  end

  def handle_cast({:delete_entry, entry_id}, todo_list) do
    TodoList.delete_entry(todo_list, entry_id)
  end

  def handle_cast(_message, todo_list) do
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
