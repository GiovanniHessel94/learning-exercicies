defmodule Todo.System do
  @moduledoc """
  System supervisor responsible for starting and supervising the entire to-do system.
  """

  use Supervisor

  @doc """
  Starts the to-do system.
  """
  @spec start_link :: Supervisor.on_start()
  def start_link, do: Supervisor.start_link(__MODULE__, nil)

  @doc """
  Initializes the to-do system.
  """
  @impl Supervisor
  @spec init(term()) :: {:ok, term()}
  def init(_init_arg), do: Supervisor.init(children(), strategy: :one_for_one)

  @spec children() :: [module()]
  defp children do
    [
      {Task.Supervisor, name: Todo.Task.Supervisor},
      Todo.Metrics,
      Todo.RegistryProcess,
      Todo.Database,
      Todo.Cache,
      Todo.Web
    ]
  end
end
