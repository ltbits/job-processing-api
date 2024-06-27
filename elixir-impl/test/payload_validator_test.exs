defmodule JobProcessor.PayloadValidatorTest do

  use ExUnit.Case, async: true
  alias JobProcessor.PayloadValidator

  test "PayloadValidator.validate_input – no args" do
    assert PayloadValidator.validate_input() == false
  end

  test "PayloadValidator.validate_input – wrong arg – list" do
    assert PayloadValidator.validate_input([]) == false
  end

  test "PayloadValidator.validate_input – wrong arg - tuple" do
    assert PayloadValidator.validate_input({}) == false
  end

  test "PayloadValidator.validate_input – wrong arg – number" do
    assert PayloadValidator.validate_input(2.0) == false
  end

  test "PayloadValidator.validate_input – empty payload object is NOT OK" do
    assert PayloadValidator.validate_input(%{}) == false
  end

  test "PayloadValidator.validate_input – using atoms as keys is NOT OK" do
    assert PayloadValidator.validate_input(%{ :tasks => [] }) == false
  end

  test "PayloadValidator.validate_input – empty tasks is OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [] }) == true
  end

  test "PayloadValidator.validate_input – using atoms as task's keys is NOT OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ :command => "ls", :name => "task-x" }
    ] }) == false
  end

  test "PayloadValidator.validate_input – missing command inside a task is NOT OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "name" => "task-x" }
    ] }) == false
  end

  test "PayloadValidator.validate_input – missing name inside a task is NOT OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls" }
    ] }) == false
  end

  test "PayloadValidator.validate_input – non-string value for tasl command is NOT OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => [], "name" => "task-x" }
    ] }) == false
  end

  test "PayloadValidator.validate_input – non-string value for task name is NOT OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls", "name" => 2 }
    ] }) == false
  end

  test "PayloadValidator.validate_input – non-existing requires property is OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls", "name" => "task-x" }
    ] }) == true
  end

  test "PayloadValidator.validate_input – if requires property exists it must be a list" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls", "name" => "task-x", "requires" => { "task-y" } }
    ] }) == false
  end

  test "PayloadValidator.validate_input – non-existing response_type property is OK" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls", "name" => "task-x" }
    ] }) == true
  end

  test "PayloadValidator.validate_input – if response_type property exists it must be a string" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls", "name" => "task-x" }
    ], "response_type" => :bash }) == false
  end

  test "PayloadValidator.validate_input – only valid task names are allowed inside require" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls", "name" => "task-x", },
      %{ "command" => "ls", "name" => "task-y", "requires" => ["abc"] }
    ] }) == false
  end

  test "PayloadValidator.validate_input – self reference is not allowed inside require" do
    assert PayloadValidator.validate_input(%{ "tasks" => [
      %{ "command" => "ls", "name" => "task-x", },
      %{ "command" => "ls", "name" => "task-y", "requires" => ["task-y"] }
    ] }) == false
  end

end