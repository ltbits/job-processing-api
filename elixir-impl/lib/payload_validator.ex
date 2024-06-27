defmodule JobProcessor.PayloadValidator do
  @moduledoc """
  Validates the input payload for job processing.
  """

  def validate_input(%{"tasks" => tasks} = input) when is_list(tasks) do
    with true <- Enum.all?(tasks, &validate_task/1),
         true <- validate_optional_root_fields(input),
         true <- validate_required_tasks(tasks) do
      true
    else
      _ -> false
    end
  end

  def validate_input(_), do: false

  def validate_input, do: false



  defp validate_optional_root_fields(input) do
    Enum.all?(["response_type"], &validate_root_field(input, &1))
  end

  defp validate_root_field(input, "response_type") do
    case Map.get(input, "response_type") do
      value when is_binary(value) or is_nil(value) -> true
      _ -> false
    end
  end

  defp validate_root_field(_, _), do: true



  defp validate_task(%{"command" => command, "name" => name} = task)
       when is_binary(command) and is_binary(name) do
    case Map.get(task, "requires") do
      nil -> true
      requires when is_list(requires) -> true
      _ -> false
    end
  end

  defp validate_task(_), do: false



  defp validate_required_tasks(tasks) do
    task_names = MapSet.new(tasks, & &1["name"])
    
    Enum.all?(tasks, fn %{"name" => task_name} = task ->
      validate_task_requires(task["requires"], task_name, task_names)
    end)
  end

  defp validate_task_requires(nil, _, _), do: true

  defp validate_task_requires(requires, task_name, all_task_names) when is_list(requires) do
    requires_set = MapSet.new(requires)
    task_name not in requires and MapSet.subset?(requires_set, all_task_names)
  end

  defp validate_task_requires(_, _, _), do: false

end