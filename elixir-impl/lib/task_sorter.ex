defmodule JobProcessor.TaskSorter do
  @moduledoc """
  Sorts tasks based on their dependencies.
  """

  def sort(tasks) when is_list(tasks) do
    with graph <- build_tasks_graph(tasks),
         sorted_task_names when is_list(sorted_task_names) <- Graph.topsort(graph) do
      map_sorted_names_to_tasks(sorted_task_names, tasks)
    else
      _ -> false
    end
  end

  defp build_tasks_graph(tasks) do
    tasks
    |> Enum.reduce(Graph.new(), fn task, graph ->
      graph = Graph.add_vertex(graph, task["name"], task)
      requires = Map.get(task, "requires", [])
      add_edges(graph, task["name"], requires)
    end)
  end

  defp add_edges(graph, task_name, requires) do
    Enum.reduce(requires, graph, fn dep, graph ->
      Graph.add_edge(graph, dep, task_name)
    end)
  end

  defp map_sorted_names_to_tasks(sorted_task_names, tasks) do
    task_map = Map.new(tasks, &{&1["name"], &1})
    Enum.map(sorted_task_names, &Map.get(task_map, &1))
  end
end
