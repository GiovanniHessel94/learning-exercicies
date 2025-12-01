defmodule Todo.Server do
  @moduledoc """
  Exercises code from the `Chapter 6 - Generic Server Processes` of the `Elixir In Action` book.

  This implementation is powered by the `GenServer` module.
  """

  use GenServer

  @typedoc """
  The `Todo.Server` supported call message types.

  Unsupported messages returns `{:error, :unsupported_message}` as response.
  """
  @type call_message_t() :: {:entries, Date.t()} | term()

  @typedoc """
  The `Todo.Server` supported cast message types.

  Unsupported messages are simply ignored.
  """
  @type cast_message_t() ::
          {:add_entry, Todo.List.entry_params_t()}
          | {:update_entry, Todo.List.id_t(), (Todo.List.entry_t() -> Todo.List.entry_t())}
          | {:delete_entry, Todo.List.id_t()}
          | term()

  ##################
  ##  Client API  ##
  ##################

  @doc """
  Returns the todo list server entries from a given date.

  Raises if the entries are not received within 5 seconds.

  ## Parameters

  - `todo_server`: The todo list server pid to get the entries from.
  - `date`: The date to get the entries from.

  ## Examples

      iex> {:ok, todo_server} = Todo.Server.start()
      {:ok, #PID<0.123.0>}

      iex> Todo.Server.add_entry(todo_server, %{date: ~D[2025-12-22], title: "Shopping"})
      :ok

      iex> Todo.Server.entries(todo_server, ~D[2025-12-22])
      [%{id: 1, date: ~D[2025-12-22], title: "Shopping"}]

      iex> Todo.Server.entries(todo_server, ~D[2025-12-22])
      ** (exit) exited in: GenServer.call(#PID<0.123.0>, {:entries, ~D[2025-12-22]}, 5000)
          ** (EXIT) time out
          (elixir 1.18.4) lib/gen_server.ex:1128: GenServer.call/3
          iex:3: (file)

  """
  @spec entries(pid(), Date.t()) :: list(Todo.List.entry_t())
  def entries(todo_server, date), do: GenServer.call(todo_server, {:entries, date})

  @doc """
  Adds a new entry to a todo list server.

  ## Parameters

  - `todo_server`: The todo list server pid to add the entry to.
  - `entry`: The entry to add.

  ## Examples

      iex> {:ok, todo_server} = Todo.Server.start()
      {:ok, #PID<0.123.0>}

      iex> Todo.Server.add_entry(todo_server, %{date: ~D[2025-12-20], title: "Dentist"})
      :ok

  """
  @spec add_entry(pid(), Todo.List.entry_params_t()) :: :ok
  def add_entry(todo_server, entry), do: GenServer.cast(todo_server, {:add_entry, entry})

  @doc """
  Updates an entry in a todo list server.

  ## Parameters

  - `todo_server`: The todo list server pid to update the entry in.
  - `entry_id`: The ID of the entry to update.
  - `updater_fun`: A function that updates the entry.

  ## Examples

      iex> {:ok, todo_server} = Todo.Server.start()
      {:ok, #PID<0.123.0>}

      iex> Todo.Server.add_entry(todo_server, %{date: ~D[2025-12-22], title: "Shopping"})
      :ok

      iex> Todo.Server.update_entry(todo_server, 1, fn entry -> %{entry | date: ~D[2025-12-21]} end)
      :ok

  """
  @spec update_entry(pid(), Todo.List.id_t(), (Todo.List.entry_t() -> Todo.List.entry_t())) :: :ok
  def update_entry(todo_server, entry_id, updater_fun) do
    GenServer.cast(todo_server, {:update_entry, entry_id, updater_fun})
  end

  @doc """
  Deletes an entry from a todo list server.

  ## Parameters

  - `todo_server`: The todo list server pid to delete the entry from.
  - `entry_id`: The ID of the entry to delete.

  ## Examples

      iex> {:ok, todo_server} = Todo.Server.start()
      {:ok, #PID<0.123.0>}

      iex> Todo.Server.add_entry(todo_server, %{date: ~D[2025-12-22], title: "Shopping"})
      :ok

      iex> Todo.Server.delete_entry(todo_server, 1)
      :ok

  """
  @spec delete_entry(pid(), Todo.List.id_t()) :: :ok
  def delete_entry(todo_server, entry_id) do
    GenServer.cast(todo_server, {:delete_entry, entry_id})
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts a new todo list server.
  """
  @spec start_link(String.t()) :: GenServer.on_start()
  def start_link(name), do: GenServer.start_link(__MODULE__, name)

  @doc """
  Initializes the todo list server state as an empty todo list.

  ...
  """
  @impl GenServer
  @spec init(String.t()) :: {:ok, {String.t(), nil}, {:continue, :init}}
  def init(name), do: {:ok, {name, nil}, {:continue, :init}}

  @doc """
  ...
  """
  @impl GenServer
  @spec handle_continue(:init, {String.t(), nil}) :: {:noreply, {String.t(), Todo.List.t()}}
  def handle_continue(:init, {name, nil}) do
    todo_list = Todo.Database.get(name) || Todo.List.new()
    {:noreply, {name, todo_list}}
  end

  @doc """
  Handles a call message.

  ## Parameters

  - `message`: The call message to handle. Reply `{:error, :unsupported_message}` when message is unsupported.
  - `from`: The caller pid and term tuple. Ignored for the moment.
  - `todo_list`: The todo list state from the server.

  """
  @impl GenServer
  @spec handle_call(call_message_t(), {pid(), term()}, {String.t(), Todo.List.t()}) ::
          {
            :reply,
            response :: list(Todo.List.entry_t()) | {:error, :unsupported_message},
            {String.t(), Todo.List.t()}
          }
  def handle_call({:entries, date}, _from, {name, todo_list}) do
    {:reply, Todo.List.entries(todo_list, date), {name, todo_list}}
  end

  def handle_call(_message, _from, {name, todo_list}) do
    {:reply, {:error, :unsupported_message}, {name, todo_list}}
  end

  @doc """
  Handles a cast message.

  ## Parameters

  - `message`: The cast message to handle.
  - `todo_list`: The todo list state from the server.

  """
  @impl GenServer
  @spec handle_cast(
          cast_message_t(),
          {String.t(), Todo.List.t()}
        ) :: {:noreply, {String.t(), Todo.List.t()}}
  def handle_cast({:add_entry, entry}, {name, todo_list}) do
    todo_list = Todo.List.add_entry(todo_list, entry)
    Todo.Database.store(name, todo_list)
    {:noreply, {name, todo_list}}
  end

  def handle_cast({:update_entry, entry_id, updater_fun}, {name, todo_list}) do
    todo_list = Todo.List.update_entry(todo_list, entry_id, updater_fun)
    Todo.Database.store(name, todo_list)
    {:noreply, {name, todo_list}}
  end

  def handle_cast({:delete_entry, entry_id}, {name, todo_list}) do
    todo_list = Todo.List.delete_entry(todo_list, entry_id)
    Todo.Database.store(name, todo_list)
    {:noreply, {name, todo_list}}
  end

  def handle_cast(_message, {name, todo_list}) do
    {:noreply, {name, todo_list}}
  end
end
