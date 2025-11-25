defmodule Todo.Database do
  @moduledoc """
  Database module responsible for starting a pool of database workers and delegating operations to them.
  All data is stored under the `./persist` folder.
  """

  use GenServer

  @db_folder "./persist"
  @pool_size 3

  ##################
  ##  Client API  ##
  ##################

  @doc """
  Stores data in the file system under the given key.

  Internally call the database server to obtain the worker pid for the given key.

  ## Parameters

  - `key`: The key to store the data under.
  - `data`: The data to store.

  ## Examples

      iex> Todo.Database.store("Bob's list",  %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}})
      :ok

  """
  @spec store(String.t(), term()) :: :ok
  def store(key, data) do
    __MODULE__
    |> GenServer.call({:worker, key})
    |> GenServer.cast({:store, key, data})
  end

  @doc """
  Retrieves data from the file system under the given key.

  Internally call the database server to obtain the worker pid for the given key.

  ## Parameters

  - `key`: The key to retrieve the data from.

  ## Examples

      iex> Todo.Database.get("Bob's list")
      %{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}

      iex> Todo.Database.get("Alice's list")
      nil

  """
  @spec get(String.t()) :: term() | nil
  def get(key) do
    __MODULE__
    |> GenServer.call({:worker, key})
    |> GenServer.call({:get, key})
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts a new database server.
  """
  @spec start() :: GenServer.on_start()
  def start, do: GenServer.start(__MODULE__, nil, name: __MODULE__)

  @doc """
  Initializes the database server state.

  It will also:
  - Create the database folder if it doesn't exist.
  - Start a pool of database workers.

  """
  @impl GenServer
  @spec init(term()) :: {:ok, nil}
  def init(_init_arg) do
    File.mkdir_p!(@db_folder)

    Enum.reduce_while(1..@pool_size, {:ok, %{}}, &start_worker/2)
  end

  @doc """
  Handles a call message.

  ## Parameters

  - `message`: The call message to handle. Currently supports only `{:worker, key}`.
  - `from`: The caller pid and term tuple. Ignored from the moment.
  - `state`: The database server state, i.e. the pool map.

  """
  @impl GenServer
  @spec handle_call(term(), {pid(), term()}, map()) :: {:reply, pid(), map()}
  def handle_call({:worker, key}, _from, state) do
    worker = Map.fetch!(state, :erlang.phash2(key, @pool_size))

    {:reply, worker, state}
  end

  @spec start_worker(integer(), {:ok, map()}) :: {:cont, {:ok, map()}} | {:halt, {:stop, term()}}
  defp start_worker(index, {:ok, acc}) do
    case Todo.DatabaseWorker.start(@db_folder) do
      {:ok, worker} -> {:cont, {:ok, Map.put(acc, index - 1, worker)}}
      {:error, reason} -> {:halt, {:stop, reason}}
    end
  end
end
