defmodule JobProcessor.TaskSorterTest do

  use ExUnit.Case, async: true
  alias JobProcessor.TaskSorter

  test "TaskSorter.sort – returns list of correctly sorted tasks" do
    assert [
        %{"name" => "task-1" },
        %{"name" => "task-3" },
        %{"name" => "task-2" },
        %{"name" => "task-4" }
    ] = TaskSorter.sort([
      %{"command" => "touch /tmp/file1", "name" => "task-1"},
      %{"command" => "cat /tmp/file1", "name" => "task-2", "requires" => ["task-3"]},
      %{"command" => "echo 'Hello World!' > /tmp/file1", "name" => "task-3", "requires" => ["task-1"]},
      %{"command" => "rm /tmp/file1", "name" => "task-4", "requires" => ["task-2", "task-3"]}
    ])
  end

  test "TaskSorter.sort – circular dependency returns false" do
    assert false == TaskSorter.sort([
      %{"command" => "touch /tmp/file1", "name" => "task-1"},
      %{"command" => "cat /tmp/file1", "name" => "task-2", "requires" => ["task-4"]},
      %{"command" => "echo 'Hello World!' > /tmp/file1", "name" => "task-3", "requires" => ["task-1"]},
      %{"command" => "rm /tmp/file1", "name" => "task-4", "requires" => ["task-2", "task-3"]}
    ])
  end

end