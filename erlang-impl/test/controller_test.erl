-module(controller_test).
-include_lib("eunit/include/eunit.hrl").

-define(TASKS_LIST, [
    #{<<"command">> => <<"touch /tmp/file1">>, <<"name">> => <<"task-1">>},
    #{<<"command">> => <<"cat /tmp/file1">>, <<"name">> => <<"task-2">>, <<"requires">> => [<<"task-3">>]},
    #{<<"command">> => <<"echo 'Hello World!' > /tmp/file1">>, <<"name">> => <<"task-3">>, <<"requires">> => [<<"task-1">>]},
    #{<<"command">> => <<"rm /tmp/file1">>, <<"name">> => <<"task-4">>, <<"requires">> => [<<"task-2">>, <<"task-3">>]}
]).

sort_test_() ->
  [
    {
      "controller:process_tasks – empty input returns error",
      fun() ->
        ?assertEqual(
          controller:generic_error_response(), 
          controller:process_tasks(#{})
        )
      end
    },
    {
      "controller:process_tasks – wrong input returns error",
      fun() ->
        ?assertEqual(
          controller:generic_error_response(), 
          controller:process_tasks(#{ <<"any">> => 1, <<"other">> => [] })
        )
      end
    },
    {
      "controller:process_tasks – empty tasks list is OK",
      fun() ->
        ?assertEqual(
          {200, #{<<"tasks">> => []}, json}, 
          controller:process_tasks(#{ <<"tasks">> => [] })
        )
      end
    },
    {
      "controller:process_tasks – wrong response_type returns JSON",
      fun() ->
        ?assertEqual(
          {200, #{<<"tasks">> => []}, json}, 
          controller:process_tasks(
            #{ <<"tasks">> => [], <<"response_type">> => <<"xml">> }
          )
        )
      end
    },
    {
      "controller:process_tasks – bash response type returns text",
      fun() ->
        {Status, Body, ContentType} = controller:process_tasks(
          #{ <<"tasks">> => [], <<"response_type">> => <<"bash">>}
        ),
        ?assertEqual(200, Status),
        ?assert(is_binary(Body)),
        ?assertEqual(text, ContentType)
      end
    },
    {
      "controller:process_tasks – tasks are properly ordered in bash response",
      fun() ->
        {_, Body, _} = controller:process_tasks(
          #{ <<"response_type">> => <<"bash">>, <<"tasks">> => ?TASKS_LIST }
        ),
        ?assertEqual(
          <<"#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1">>, 
          Body
        )
      end
    },
    {
      "controller:process_tasks – tasks are properly ordered in JSON response",
      fun() ->
        {_, Body, _} = controller:process_tasks(#{ <<"tasks">> => ?TASKS_LIST }),
        ?assertEqual(#{ <<"tasks">> => [
          #{<<"command">> => <<"touch /tmp/file1">>, <<"name">> => <<"task-1">>},
          #{<<"command">> => <<"echo 'Hello World!' > /tmp/file1">>, <<"name">> => <<"task-3">>},
          #{<<"command">> => <<"cat /tmp/file1">>, <<"name">> => <<"task-2">>},
          #{<<"command">> => <<"rm /tmp/file1">>, <<"name">> => <<"task-4">>}
        ]}, Body)
      end
    },
    {
      "controller:process_tasks – JSON response only includes command and name for each task",
      fun() ->
        {_, Body, _} = controller:process_tasks(#{<<"tasks">> => ?TASKS_LIST}),
        lists:all(fun(Task) ->
          maps:size(Task) =:= 2 andalso
          maps:is_key(<<"name">>, Task) andalso
          maps:is_key(<<"command">>, Task)
        end, maps:get(<<"tasks">>, Body))
      end
    }
  ].
