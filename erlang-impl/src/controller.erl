-module(controller).
-export([process_tasks/1, generic_error_response/0]).

%% @doc Handles the /process endpoint, processing the input tasks.
-spec process_tasks(map()) -> {integer(), any(), atom()}.
process_tasks(Input) ->
    case payload_validator:validate_input(Input) of
        true ->
            Tasks = task_sorter:sort(maps:get(<<"tasks">>, Input, [])),
            case Tasks of
                false ->
                    {400, #{error => <<"Unable to sort tasks due to cyclic dependencies.">>}, json};
                _ ->
                    ResponseType = maps:get(<<"response_type">>, Input, nil),
                    process_tasks_response(ResponseType, Tasks)
            end;
        false ->
            generic_error_response()
    end.

-spec generic_error_response() -> {integer(), map()}.
generic_error_response() ->
    {400, #{error => <<"Invalid payload.">>}}.

%% Private functions

%% @doc Generates the appropriate response based on the response_type.
-spec process_tasks_response(binary() | nil, list(map())) -> {integer(), any(), atom()}.
process_tasks_response(<<"bash">>, Tasks) ->
    {200, tasks_to_bash_script(Tasks), text};
process_tasks_response(_, Tasks) ->
    ResTasks = [maps:with([<<"command">>, <<"name">>], Task) || Task <- Tasks],
    {200, #{<<"tasks">> => ResTasks}, json}.

%% @doc Converts a list of tasks to a Bash script.
-spec tasks_to_bash_script(list(map())) -> binary().
tasks_to_bash_script(Tasks) ->
   CommandsList = [maps:get(<<"command">>, Task) || Task <- Tasks],
   <<"#!/usr/bin/env bash\n", (iolist_to_binary(lists:join("\n", CommandsList)))/binary>>.