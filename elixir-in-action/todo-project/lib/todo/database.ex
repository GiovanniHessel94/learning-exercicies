defmodule Todo.Database do
  @moduledoc """
  Database module responsible for storing an retrieving data from the file system.
  All data is stored under the `./persist` folder.
  """

  use GenServer

  @db_folder "./persist"

  ##################
  ##  Client API  ##
  ##################

  @doc """
  Stores data in the file system under the given key.

  ## Parameters

  - `key`: The key to store the data under.
  - `data`: The data to store.

  ## Examples

      iex> Todo.Database.store("Bob's list",  %TodoList{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}})
      :ok

  """
  @spec store(String.t(), term()) :: :ok
  def store(key, data), do: GenServer.cast(__MODULE__, {:store, key, data})

  @doc """
  Retrieves data from the file system under the given key.

  ## Parameters

  - `key`: The key to retrieve the data from.

  ## Examples

      iex> Todo.Database.get("Bob's list")
      %{next_id: 2, entries: %{1 => %{id: 1, date: ~D[2025-12-20], title: "Dentist"}}}

      iex> Todo.Database.get("Alice's list")
      nil

  """
  @spec get(String.t()) :: term() | nil
  def get(key), do: GenServer.call(__MODULE__, {:get, key})

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts a new database server.
  """
  @spec start() :: GenServer.on_start()
  def start, do: GenServer.start(__MODULE__, nil, name: __MODULE__)

  @doc """
  Initializes the database server state as an empty map.

  Creates the database folder if it doesn't exist.
  """
  @impl GenServer
  @spec init(term()) :: {:ok, nil}
  def init(_init_arg) do
    File.mkdir_p!(@db_folder)
    {:ok, nil}
  end

  @doc """
  Handles a cast message.

  ## Parameters

  - `message`: The cast message to handle. Currently supports only `{:store, key, data}`.
  - `state`: The database server state.

  """
  @impl GenServer
  @spec handle_cast(term(), nil) :: {:noreply, nil}
  def handle_cast({:store, key, data}, state) do
    # Using `:erlang.term_to_binary/1` makes the code vulnerable to Remote Code Execution (RCE) attacks.
    spawn(fn ->
      key
      |> file_name()
      |> File.write!(:erlang.term_to_binary(data))
    end)

    {:noreply, state}
  end

  @doc """
  Handles a call message.

  ## Parameters

  - `message`: The call message to handle. Currently supports only `{:get, key}`.
  - `from`: The caller pid and term tuple.
  - `state`: The database server state.

  """
  @impl GenServer
  @spec handle_call(term(), {pid(), term()}, nil) :: {:noreply, nil}
  def handle_call({:get, key}, from, state) do
    spawn(fn ->
      data =
        case File.read(file_name(key)) do
          {:ok, content} -> :erlang.binary_to_term(content)
          {:error, _reason} -> nil
        end

      GenServer.reply(from, data)
    end)

    {:noreply, state}
  end

  @spec file_name(String.t()) :: String.t()
  defp file_name(key), do: Path.join(@db_folder, to_string(key))
end
