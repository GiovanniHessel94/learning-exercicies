defmodule Todo.DatabaseWorker do
  @moduledoc """
  Database server responsible for storing an retrieving data from the file system.
  """

  use GenServer

  ##################
  ##  Client API  ##
  ##################

  @doc """
  Stores data in the file system under the given key.

  ## Parameters

  - `worker_id`: The database worker ID.
  - `key`: The key to store the data under.
  - `data`: The data to store.

  ## Examples

      iex> Todo.DatabaseWorker.store("Bob's list",  %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}})
      :ok

  """
  @spec store(integer(), String.t(), term()) :: :ok
  def store(worker_id, key, data), do: GenServer.cast(via_tuple(worker_id), {:store, key, data})

  @doc """
  Retrieves data from the file system under the given key.

  ## Parameters

  - `worker_id`: The database worker ID.
  - `key`: The key to retrieve the data from.

  ## Examples

      iex> Todo.DatabaseWorker.get(worker, "Bob's list")
      %{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}

      iex> Todo.DatabaseWorker.get(worker, "Alice's list")
      nil

  """
  @spec get(integer(), String.t()) :: term() | nil
  def get(worker_id, key), do: GenServer.call(via_tuple(worker_id), {:get, key})

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts a new database worker.
  """
  @spec start_link({String.t(), integer()}) :: GenServer.on_start()
  def start_link({db_folder, worker_id}) do
    GenServer.start_link(__MODULE__, db_folder, name: via_tuple(worker_id))
  end

  @doc """
  Initializes the database worker state with the given database folder.
  """
  @impl GenServer
  @spec init(String.t()) :: {:ok, String.t()}
  def init(db_folder), do: {:ok, db_folder}

  @doc """
  Handles a cast message.

  ## Parameters

  - `message`: The cast message to handle. Currently supports only `{:store, key, data}`.
  - `state`: The database worker state, i.e. the database folder.

  """
  @impl GenServer
  @spec handle_cast(term(), String.t()) :: {:noreply, String.t()}
  def handle_cast({:store, key, data}, state) do
    # Using `:erlang.term_to_binary/1` makes the code vulnerable to Remote Code Execution (RCE) attacks.
    key
    |> file_name(state)
    |> File.write!(:erlang.term_to_binary(data))

    {:noreply, state}
  end

  @doc """
  Handles a call message.

  ## Parameters

  - `message`: The call message to handle. Currently supports only `{:get, key}`.
  - `from`: The caller pid and term tuple.
  - `state`: The database worker state, i.e. the database folder.

  """
  @impl GenServer
  @spec handle_call(term(), {pid(), term()}, String.t()) :: {:noreply, String.t()}
  def handle_call({:get, key}, from, state) do
    file_name = file_name(key, state)

    data =
      case File.read(file_name) do
        {:ok, content} -> :erlang.binary_to_term(content)
        {:error, _reason} -> nil
      end

    GenServer.reply(from, data)

    {:noreply, state}
  end

  @spec via_tuple(integer()) :: {:via, Registry, {__MODULE__, integer()}}
  defp via_tuple(worker_id), do: Todo.RegistryProcess.via_tuple({__MODULE__, worker_id})

  @spec file_name(String.t(), String.t()) :: String.t()
  defp file_name(key, db_folder), do: Path.join(db_folder, to_string(key))
end
