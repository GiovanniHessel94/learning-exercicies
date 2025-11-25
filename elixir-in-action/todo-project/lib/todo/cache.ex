defmodule Todo.Cache do
  @moduledoc """
  Cache module responsible for mapping todo list names to `Todo.Server` processes.
  It starts the `Todo.Server` process if it doesn't exist for the given list name.
  """

  use GenServer

  ##################
  ##  Client API  ##
  ##################

  @doc """
  Starts or retrieves a `Todo.Server` process for the given list name.

  ## Parameters

  - `list_name`: The name of the todo list to start or retrieve a `Todo.Server` process for.

  ## Examples

      iex> {:ok, todo_server} = Todo.Cache.start_or_retrieve_server_by_list_name("Bob's list")
      {:ok, #PID<0.123.0>}

      iex> Todo.Cache.start_or_retrieve_server_by_list_name("Bob's list")
      {:ok, #PID<0.123.0>}

      iex> Todo.Cache.start_or_retrieve_server_by_list_name("Alice's list")
      {:ok, #PID<0.124.0>}

  """
  @spec start_or_retrieve_server_by_list_name(String.t()) :: {:ok, pid()}
  def start_or_retrieve_server_by_list_name(list_name) do
    GenServer.call(__MODULE__, {:start_or_retrieve_server, list_name})
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts a new cache and database servers.
  """
  @spec start() :: GenServer.on_start()
  def start do
    # Temporary approach to start the database server.
    Todo.Database.start()

    GenServer.start(__MODULE__, nil, name: __MODULE__)
  end

  @doc """
  Initializes the cache server state as an empty map.
  """
  @impl GenServer
  @spec init(term()) :: {:ok, map()}
  def init(_init_arg), do: {:ok, %{}}

  @doc """
  Handles a call message.

  ## Parameters

  - `message`: The call message to handle. Currently supports only `{:start_or_retrieve_server, list_name}`.
  - `from`: The caller pid and term tuple. Ignored for the moment.
  - `state`: The cache server state.

  """
  @impl GenServer
  @spec handle_call(term(), {pid(), term()}, map()) :: {:reply, term(), map()}
  def handle_call({:start_or_retrieve_server, list_name}, _from, state) do
    {server, state} =
      Map.get_and_update(state, list_name, &start_or_retrieve_server(&1, list_name))

    {:reply, server, state}
  end

  @spec start_or_retrieve_server(nil | pid(), String.t()) :: {pid(), pid()}
  defp start_or_retrieve_server(nil, list_name) do
    {:ok, server} = Todo.Server.start(list_name)
    {server, server}
  end

  defp start_or_retrieve_server(server, _list_name), do: {server, server}
end
