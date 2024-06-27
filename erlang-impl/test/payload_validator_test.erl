-module(payload_validator_test).
-include_lib("eunit/include/eunit.hrl").

validate_input_test_() ->
  [
    {
      "PayloadValidator.validate_input – no args",
      ?_assertEqual(false, payload_validator:validate_input())
    },
    {
      "PayloadValidator.validate_input – wrong arg – list",
      ?_assertEqual(false, payload_validator:validate_input([]))
    },
    {
      "PayloadValidator.validate_input – wrong arg - tuple",
      ?_assertEqual(false, payload_validator:validate_input({}))
    },
    {
      "PayloadValidator.validate_input – wrong arg – number",
      ?_assertEqual(false, payload_validator:validate_input(2.0))
    },
    {
      "PayloadValidator.validate_input – empty payload object is NOT OK",
      ?_assertEqual(false, payload_validator:validate_input(#{}))
    },
    {
      "PayloadValidator.validate_input – using atoms as keys is NOT OK",
      ?_assertEqual(false, payload_validator:validate_input(#{tasks => []}))
    },
    {
      "PayloadValidator.validate_input – empty tasks is OK",
      ?_assertEqual(true, payload_validator:validate_input(#{<<"tasks">> => []}))
    },
    {
      "PayloadValidator.validate_input – using atoms as task's keys is NOT OK",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"tasks">> => [
          #{command => <<"ls">>, name => <<"task-x">>}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – missing command inside a task is NOT OK",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"tasks">> => [#{<<"name">> => <<"task-x">>}]}
      ))
    },
    {
      "PayloadValidator.validate_input – missing name inside a task is NOT OK",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"tasks">> => [#{<<"command">> => <<"ls">>}]}
      ))
    },
    {
      "PayloadValidator.validate_input – non-string value for task command is NOT OK",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"tasks">> => [
          #{<<"command">> => [], <<"name">> => <<"task-x">>}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – non-string value for task name is NOT OK",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"tasks">> => [
          #{<<"command">> => <<"ls">>}, 
          #{<<"name">> => 2}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – non-existing requires property is OK",
      ?_assertEqual(true, payload_validator:validate_input(
        #{<<"tasks">> => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – if requires property exists it must be a list",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"tasks">> => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>, 
            <<"requires">> => {<<"task-y">>}}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – non-existing response_type property is OK",
      ?_assertEqual(true, payload_validator:validate_input(
        #{<<"tasks">> => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – if response_type property exists it must be a string",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"response_type">> => bash, <<"tasks">> => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – only valid task names are allowed inside require",
      ?_assertEqual(false, payload_validator:validate_input(
        #{<<"tasks">> => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>},
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-y">>, <<"requires">> => [<<"abc">>]}
        ]}
      ))
    },
    {
      "PayloadValidator.validate_input – self reference is not allowed inside require",
      ?_assertEqual(false, payload_validator:validate_input(
        #{"tasks" => [
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-x">>},
          #{<<"command">> => <<"ls">>, <<"name">> => <<"task-y">>, <<"requires">> => [<<"task-y">>]}
        ]}
      ))
    }
  ].
