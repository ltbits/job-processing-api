-module(task_sorter_test).
-include_lib("eunit/include/eunit.hrl").

sort_test_() ->
  [
    {
      "TaskSorter.sort – returns list of correctly sorted tasks",
      fun() ->
        Expected = [
          #{<<"command">> => <<"touch /tmp/file1">>, <<"name">> => <<"task-1">>},
          #{<<"command">> => <<"echo 'Hello World!' > /tmp/file1">>, <<"name">> => <<"task-3">>, <<"requires">> => [<<"task-1">>]},
          #{<<"command">> => <<"cat /tmp/file1">>, <<"name">> => <<"task-2">>, <<"requires">> => [<<"task-3">>]},
          #{<<"command">> => <<"rm /tmp/file1">>, <<"name">> => <<"task-4">>, <<"requires">> => [<<"task-2">>, <<"task-3">>]}
        ],
        Actual = task_sorter:sort([
          #{<<"command">> => <<"touch /tmp/file1">>, <<"name">> => <<"task-1">>},
          #{<<"command">> => <<"cat /tmp/file1">>, <<"name">> => <<"task-2">>, <<"requires">> => [<<"task-3">>]},
          #{<<"command">> => <<"echo 'Hello World!' > /tmp/file1">>, <<"name">> => <<"task-3">>, <<"requires">> => [<<"task-1">>]},
          #{<<"command">> => <<"rm /tmp/file1">>, <<"name">> => <<"task-4">>, <<"requires">> => [<<"task-2">>, <<"task-3">>]}
        ]),
        ?assertEqual(Expected, Actual)
      end
    },
    {
      "TaskSorter.sort – circular dependency returns false",
      fun() ->
        Actual = task_sorter:sort([
          #{<<"command">> => <<"touch /tmp/file1">>, <<"name">> => <<"task-1">>},
          #{<<"command">> => <<"cat /tmp/file1">>, <<"name">> => <<"task-2">>, <<"requires">> => [<<"task-4">>]},
          #{<<"command">> => <<"echo 'Hello World!' > /tmp/file1">>, <<"name">> => <<"task-3">>, <<"requires">> => [<<"task-1">>]},
          #{<<"command">> => <<"rm /tmp/file1">>, <<"name">> => <<"task-4">>, <<"requires">> => [<<"task-2">>, <<"task-3">>]}
        ]),
        ?assertEqual(false, Actual)
      end
    }
  ].
