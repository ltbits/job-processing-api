defmodule JobProcessor.ControllerTest do

  use ExUnit.Case, async: true
  alias JobProcessor.Controller

  @tasks_list [
    %{"command" => "touch /tmp/file1", "name" => "task-1"},
    %{"command" => "cat /tmp/file1", "name" => "task-2", "requires" => ["task-3"]},
    %{"command" => "echo 'Hello World!' > /tmp/file1", "name" => "task-3", "requires" => ["task-1"]},
    %{"command" => "rm /tmp/file1", "name" => "task-4", "requires" => ["task-2", "task-3"]}
  ]

  test "Controller.process_tasks – empty input returns error" do
    assert Controller.process_tasks(%{}) == Controller.generic_error_response()
  end

  test "Controller.process_tasks – wrong input returns error" do
    assert Controller.process_tasks(%{ "any" => 1, "other" => [] }) == Controller.generic_error_response()
  end

  test "Controller.process_tasks – empty tasks list is OK" do
    assert Controller.process_tasks(%{ "tasks" => [] }) == {200, %{"tasks" => []}, :json}
  end

  test "Controller.process_tasks – wrong response_type returns JSON" do
    assert Controller.process_tasks(%{ "tasks" => [], "response_type" => "xml" }) == {200, %{"tasks" => []}, :json}
  end

  test "Controller.process_tasks – bash response type returns text" do
    {status, body, content_type} = Controller.process_tasks(%{ "tasks" => [], "response_type" => "bash" })
    assert status == 200 and is_binary(body) and content_type == :text
  end

  test "Controller.process_tasks – tasks are properly ordered in bash response" do
    {_, body, _} = Controller.process_tasks(%{ "response_type" => "bash", "tasks" => @tasks_list })
    assert body == "#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1" 
  end

  test "Controller.process_tasks – tasks are properly ordered in JSON response" do
    {_, body, _} = Controller.process_tasks(%{ "tasks" => @tasks_list })
    assert %{ "tasks" => [
      %{"name" => "task-1" },
      %{"name" => "task-3" },
      %{"name" => "task-2" },
      %{"name" => "task-4" }
    ]} = body
  end

  test "Controller.process_tasks – JSON response only includes command and name for each task" do
    {_, body, _} = Controller.process_tasks(%{ "tasks" => @tasks_list })
    valid_keys = MapSet.new(["name", "command"])
    assert Enum.all?(body["tasks"], fn task -> 
      MapSet.new(Map.keys(task)) == valid_keys
    end)
  end

end