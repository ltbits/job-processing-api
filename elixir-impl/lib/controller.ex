defmodule JobProcessor.Controller do
  @moduledoc false

  alias JobProcessor.PayloadValidator
  alias JobProcessor.TaskSorter

  def process_tasks(input) do
    with true <- PayloadValidator.validate_input(input),
         tasks when is_list(tasks) <- TaskSorter.sort(input["tasks"]) do
      case input["response_type"] do
        "bash" -> {200, build_bash_command_list(tasks), :text}
        _ -> 
          res_tasks = Enum.map(tasks, &Map.take(&1, ["name", "command"]))
          {200, %{"tasks" => res_tasks}, :json}
      end
    else
      false -> generic_error_response()
    end
  end

  defp build_bash_command_list(tasks) do
    commands = Enum.map_join(tasks, "\n", & &1["command"])
    "#!/usr/bin/env bash\n#{commands}"
  end

  def generic_error_response do
    { 400, %{ :error => "Invalid payload." } }
  end

end
