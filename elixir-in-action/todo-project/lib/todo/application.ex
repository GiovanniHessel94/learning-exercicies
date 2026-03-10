defmodule Todo.Application do
  @moduledoc """
  Application module responsible for starting the todo system.
  """
  use Application

  @doc """
  Starts the todo system.
  """
  @impl Application
  @spec start(term(), term()) :: Supervisor.on_start()
  def start(_type, _args), do: Todo.System.start_link()
end
