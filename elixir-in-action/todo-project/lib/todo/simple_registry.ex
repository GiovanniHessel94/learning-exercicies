defmodule Todo.SimpleRegistry do
  @moduledoc """
  Simple registry server responsible for managing and mapping process PIDs.

  The implementation is powered by the `GenServer` and `:ets` modules.
  """

  use GenServer

  ##################
  ##  Client API  ##
  ##################

  @doc """
  Registers the caller PID under the given name using the registry ETS table.

  Links the caller to the registry server in order to cleanup the ETS table on caller's termination.

  ## Parameters

  - `name`: The name to register the caller PID under.

  ## Examples

      iex> Todo.SimpleRegistry.register(:my_process)
      :ok

      iex> Todo.SimpleRegistry.register(:my_process)
      :error

  """
  @spec register(term()) :: :ok | :error
  def register(name) do
    case :ets.insert_new(__MODULE__, {name, self()}) do
      true ->
        Process.link(Process.whereis(__MODULE__))
        :ok

      false ->
        :error
    end
  end

  @doc """
  Retrieves the PID under the given name from the registry ETS table.

  ## Parameters

  - `name`: The name to retrieve the PID from.

  ## Examples

      iex> Todo.SimpleRegistry.whereis(:my_process)
      #PID<0.123.0>

      iex> Todo.SimpleRegistry.whereis(:my_process)
      nil

  """
  @spec whereis(term()) :: pid() | nil
  def whereis(name) do
    case :ets.lookup(__MODULE__, name) do
      [{^name, pid}] -> pid
      [] -> nil
    end
  end

  ##################
  ##  Server API  ##
  ##################

  @doc """
  Starts the simple registry server.
  """
  @spec start_link :: GenServer.on_start()
  def start_link, do: GenServer.start_link(__MODULE__, %{}, name: __MODULE__)

  @doc """
  Initializes the simple registry server state, sets the trap exits flag and creates the registry ETS table.
  """
  @impl GenServer
  @spec init(term()) :: {:ok, term(), {:continue, :init}}
  def init(_init_arg) do
    Process.flag(:trap_exit, true)

    :ets.new(__MODULE__, [:set, :named_table, :public])

    {:ok, nil}
  end

  @doc """
  Handles the `{:EXIT, pid, reason}` message.

  Removes entries from the registry ETS table when linked processes exit.

  ## Parameters

  - `message`: The `{:EXIT, pid, reason}` message to handle.
  - `state`: The simple registry server state.

  """
  @impl GenServer
  @spec handle_info(term(), term()) :: {:noreply, term()}
  def handle_info({:EXIT, pid, _reason}, state) do
    :ets.match_delete(__MODULE__, {:_, pid})

    {:noreply, state}
  end
end
