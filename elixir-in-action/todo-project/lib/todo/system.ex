defmodule Todo.System do
  @moduledoc """
  System module responsible for starting and supervising the entire to-do system.
  """

  use Supervisor

  @doc """
  Starts the to-do system.
  """
  @spec start_link() :: Supervisor.on_start()
  def start_link, do: Supervisor.start_link(__MODULE__, nil)

  @doc """
  Initializes the to-do system.
  """
  @impl Supervisor
  @spec init(term()) :: {:ok, term()}
  def init(_init_arg), do: Supervisor.init([Todo.Cache], strategy: :one_for_one)
end
