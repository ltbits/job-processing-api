-module(payload_validator_test).
-include_lib("eunit/include/eunit.hrl").

validate_input_test_() ->
  [
    {
      "PayloadValidator.validate_input – no args",
      fun() ->
        Expected = {error, error_message:payload_empty()},
        Actual   = payload_validator:validate_input(),
        ?assert(Expected =:= Actual)
      end
    },
    {
      "PayloadValidator.validate_input – wrong arg – list",
      fun() ->
        Expected = {error, error_message:payload_invalid_tasks_arg()},
        Actual   = payload_validator:validate_input([]),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – wrong arg - tuple",
      fun() ->
        Expected = {error, error_message:payload_invalid_tasks_arg()},
        Actual   = payload_validator:validate_input({}),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – wrong arg – number",
      fun() ->
        Expected = {error, error_message:payload_invalid_tasks_arg()},
        Actual   = payload_validator:validate_input(2.0),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – empty payload object is NOT OK",
      fun() ->
        Expected = {error, error_message:payload_invalid_tasks_arg()},
        Actual   = payload_validator:validate_input(#{}),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – using atoms as keys is NOT OK",
      fun() ->
        Expected = {error, error_message:payload_invalid_tasks_arg()},
        Actual   = payload_validator:validate_input(#{tasks => []}),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – empty tasks is OK",
      fun() ->
        Input = #{<<"tasks">> => []},
        Expected = {ok, Input},
        Actual = payload_validator:validate_input(Input),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – using atoms as task's keys is NOT OK",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_command_or_name()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [
            #{command => <<"ls">>, name => <<"task-x">>}
          ]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – missing command inside a task is NOT OK",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_command_or_name()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [#{<<"name">> => <<"task-x">>}]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – missing name inside a task is NOT OK",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_command_or_name()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [#{<<"command">> => <<"ls">>}]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – non-string value for task command is NOT OK",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_command_or_name()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [
            #{<<"command">> => [], <<"name">> => <<"task-x">>}
          ]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – non-string value for task name is NOT OK",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_command_or_name()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [
            #{<<"command">> => <<"ls">>}, #{<<"name">> => 2}
          ]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – non-existing requires property is OK",
      fun() ->
        Input = #{<<"tasks">> => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>}
        ]},
        Expected = {ok, Input},
        Actual = payload_validator:validate_input(Input),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – if requires property exists it must be a list",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_requires()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [
            #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>, 
              <<"requires">> => {<<"task-y">>}}
          ]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – non-existing response_type property is OK",
      fun() ->
        Input = #{<<"tasks">> => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>}
        ]},
        Expected = {ok, Input},
        Actual = payload_validator:validate_input(Input),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – if response_type property exists it must be a string",
      fun() ->
        Expected = {error, error_message:payload_invalid_response_type()},
        Actual   = payload_validator:validate_input(
          #{<<"response_type">> => bash, <<"tasks">> => [
            #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>}
          ]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – only valid task names are allowed inside require",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_name_in_requires()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [
            #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>},
            #{<<"command">> => <<"ls">>, <<"name">> => <<"task-y">>, <<"requires">> => [<<"abc">>]}
          ]}
        ),
        ?assertMatch(Expected, Actual)
      end
    },
    {
      "PayloadValidator.validate_input – self reference is not allowed inside require",
      fun() ->
        Expected = {error, error_message:payload_invalid_task_name_in_requires()},
        Actual   = payload_validator:validate_input(
          #{<<"tasks">> => [
            #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>},
            #{<<"command">> => <<"ls">>, <<"name">> => <<"task-y">>, <<"requires">> => [<<"task-y">>]}
          ]}
        ),
        ?assertMatch(Expected, Actual)
      end
    }
  ].
