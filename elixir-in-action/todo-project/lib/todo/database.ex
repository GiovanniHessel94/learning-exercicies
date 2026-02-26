defmodule Todo.Database do
  @moduledoc """
  Database supervisor responsible for starting a pool of database workers and delegating operations to them.
  All data is stored under the `./persist` folder.
  """

  @db_folder "./persist"
  @pool_size 3

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
  def store(key, data) do
    key
    |> choose_worker()
    |> Todo.DatabaseWorker.store(key, data)
  end

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
  def get(key) do
    key
    |> choose_worker()
    |> Todo.DatabaseWorker.get(key)
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts the database supervisor.

  It will:
  - Create the database folder if it doesn't exist.
  - Start a pool of database workers as children.

  """
  @spec start_link :: Supervisor.on_start()
  def start_link do
    File.mkdir_p!(@db_folder)

    1..@pool_size
    |> Enum.map(&worker_spec/1)
    |> Supervisor.start_link(strategy: :one_for_one)
  end

  @doc """
  Builds the child spec for the database supervisor.
  """
  @spec child_spec(term()) :: Supervisor.child_spec()
  def child_spec(_opts) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, []}, type: :supervisor}
  end

  @spec choose_worker(String.t()) :: integer()
  defp choose_worker(key), do: :erlang.phash2(key, @pool_size) + 1

  @spec worker_spec(integer()) :: Supervisor.child_spec()
  defp worker_spec(worker_id) do
    Supervisor.child_spec({Todo.DatabaseWorker, {@db_folder, worker_id}}, id: worker_id)
  end
end
