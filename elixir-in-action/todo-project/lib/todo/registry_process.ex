defmodule Todo.RegistryProcess do
  @moduledoc """
  Registry process responsible for managing and mapping process PIDs.
  """

  @doc """
  Starts the registry process.
  """
  @spec start_link :: {:ok, pid()} | {:error, term()}
  def start_link, do: Registry.start_link(keys: :unique, name: __MODULE__)

  @doc """
  Builds the child spec for the registry process.
  """
  @spec child_spec(term()) :: Supervisor.child_spec()
  def child_spec(_opts) do
    Supervisor.child_spec(Registry, id: __MODULE__, start: {__MODULE__, :start_link, []})
  end

  @doc """
  Helper function that builds a via tuple to register and identify processes in the registry.
  """
  @spec via_tuple(term()) :: {:via, Registry, {__MODULE__, term()}}
  def via_tuple(key), do: {:via, Registry, {__MODULE__, key}}
end
