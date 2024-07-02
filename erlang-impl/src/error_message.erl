-module(error_message).
-compile(export_all).

generic_error_message() ->
  <<"invalid-payload">>.

payload_empty() ->
  <<"no-tasks-given">>.

payload_invalid_tasks_arg() ->
  <<"invalid-tasks-list">>.

payload_invalid_response_type() ->
  <<"invalid-response-type">>.

payload_invalid_root_fields() ->
  <<"invalid-optional-root-fields">>.

payload_invalid_task_command_or_name() ->
  <<"invalid-task-command-or-name">>.

payload_invalid_task_requires() ->
  <<"invalid-task-requires">>.

payload_invalid_task_name_in_requires() ->
  <<"invalid-task-name-in-requires">>.

circular_dependency() ->
  <<"circular-dependency">>.