defmodule Todo.Database do
  @moduledoc """
  Database supervisor responsible for starting a pool of database workers and delegating operations to them.
  All data is stored under the `./persist` folder.
  """

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
    :poolboy.transaction(__MODULE__, &Todo.DatabaseWorker.store(&1, key, data))
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
    :poolboy.transaction(__MODULE__, &Todo.DatabaseWorker.get(&1, key))
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Builds the child spec for the database supervisor.
  """
  @spec child_spec(term()) :: :supervisor.child_spec()
  def child_spec(_opts) do
    db_folder = db_folder()
    File.mkdir_p!(db_folder)

    :poolboy.child_spec(
      __MODULE__,
      [
        name: {:local, __MODULE__},
        worker_module: Todo.DatabaseWorker,
        size: @pool_size
      ],
      db_folder: db_folder
    )
  end

  defp db_folder, do: Application.fetch_env!(:todo, :db_folder)
end
