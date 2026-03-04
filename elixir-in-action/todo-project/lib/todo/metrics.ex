defmodule Todo.Metrics do
  @moduledoc """
  Task responsible for collecting and logging metrics about the system every minute.
  """

  use Task

  require Logger

  @one_minute 60_000

  @doc """
  Starts the metrics task.
  """
  @spec start_link(term()) :: {:ok, pid()}
  def start_link(_init_arg), do: Task.start_link(&loop/0)

  @spec loop :: no_return()
  defp loop do
    Task.Supervisor.async_nolink(Todo.Task.Supervisor, &collect_and_log_metrics/0)

    Process.sleep(@one_minute)

    loop()
  end

  @spec collect_and_log_metrics :: :ok
  def collect_and_log_metrics do
    metrics = [
      memory_usage: :erlang.memory(:total),
      process_count: :erlang.system_info(:process_count)
    ]

    Logger.info(inspect(metrics))
  end
end
