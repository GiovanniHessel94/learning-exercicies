defmodule Todo.Cache do
  @moduledoc """
  Cache supervisor responsible for mapping todo list names to `Todo.Server` processes.
  It starts the `Todo.Server` process if it doesn't exist for the given list name.
  """

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
    # Temporary approach to starting a list server.
    case start_server(list_name) do
      {:ok, pid} -> {:ok, pid}
      {:error, {:already_started, pid}} -> {:ok, pid}
    end
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts the cache supervisor.
  """
  @spec start_link :: Supervisor.on_start()
  def start_link do
    DynamicSupervisor.start_link(name: __MODULE__, strategy: :one_for_one)
  end

  @doc """
  Builds the child spec for the cache supervisor.
  """
  @spec child_spec(term()) :: Supervisor.child_spec()
  def child_spec(_opts) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, []}, type: :supervisor}
  end

  @spec start_server(String.t()) :: {:ok, pid()} | {:error, term()}
  defp start_server(list_name) do
    DynamicSupervisor.start_child(__MODULE__, {Todo.Server, list_name})
  end
end
