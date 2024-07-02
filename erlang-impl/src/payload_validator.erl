-module(payload_validator).
-export([validate_input/0, validate_input/1]).

%% @doc Validates the input payload.
-spec validate_input(Input :: map()) -> {ok, map()} | {error, binary()}.
validate_input(Input) when is_map(Input) ->
    case maps:get(<<"tasks">>, Input, undefined) of
        Tasks when is_list(Tasks) ->
            case maps:get(<<"response_type">>, Input, undefined) of
                undefined -> 
                    validate_input_by_tasks(Input, Tasks);
                ResponseType when is_binary(ResponseType) -> 
                    validate_input_by_tasks(Input, Tasks);
                _ -> 
                    {error, error_message:payload_invalid_response_type()}
            end;
        _ -> 
            {error, error_message:payload_invalid_tasks_arg()}
    end;
validate_input(_) -> 
    {error, error_message:payload_invalid_tasks_arg()}.

-spec validate_input() -> {error, binary()}.
validate_input() -> 
    {error, error_message:payload_empty()}.

-spec validate_input_by_tasks(Input :: map(), Tasks :: list()) -> {ok, map()} | {error, binary()}.
validate_input_by_tasks(Input, Tasks) ->
    case validate_tasks(Tasks) of
        {ok, Tasks} ->
            {ok, Input};
        Error ->
            Error
    end.

%% @doc Checks that command and name are present for each task and they have binary values.
-spec validate_tasks(Tasks :: list()) -> {ok, list()} | {error, binary()}.
validate_tasks(Tasks) ->
    validate_tasks(Tasks, []).

-spec validate_tasks(Tasks :: list(), ValidTasks :: list()) -> {ok, list()} | {error, binary()}.
validate_tasks([], ValidTasks) ->
    % Tasks = lists:reverse(ValidTasks),
    case validate_tasks_by_requires_list(ValidTasks) of
        {ok, Tasks} ->
            {ok, Tasks};
        Error ->
            Error
    end;
validate_tasks([Task | Tasks], ValidTasks) ->
    case validate_task(Task) of
        {ok, ValidTask} ->
            validate_tasks(Tasks, [ValidTask | ValidTasks]);
        Error ->
            Error
    end.

-spec validate_task(Task :: map()) -> {ok, map()} | {error, binary()}.
validate_task(#{<<"command">> := Command, <<"name">> := Name} = Task) when is_binary(Command), is_binary(Name) ->
    {ok, Task};
validate_task(_) -> 
    {error, error_message:payload_invalid_task_command_or_name()}.


%% @doc Checks that requires list if present for a task has valid task names specified.
-spec validate_tasks_by_requires_list(Tasks :: [map()]) -> {ok, [map()]} | {error, binary()}.
validate_tasks_by_requires_list(Tasks) ->
    AllTaskNames = sets:from_list([maps:get(<<"name">>, Task) || Task <- Tasks]),
    validate_tasks_by_requires_list(Tasks, [], AllTaskNames).

-spec validate_tasks_by_requires_list(Tasks :: [map()], ValidTasks :: [map()], AllTaskNames :: sets:set()) -> {ok, [map()]} | {error, binary()}.
validate_tasks_by_requires_list([], ValidTasks, _AllTaskNames) ->
    {ok, ValidTasks};
validate_tasks_by_requires_list([Task | Tasks], ValidTasks, AllTaskNames) ->
    case validate_task_requires_list(Task, AllTaskNames) of
        {ok, ValidTask} ->
            validate_tasks_by_requires_list(Tasks, [ValidTask | ValidTasks], AllTaskNames);
        Error ->
            Error
    end.

-spec validate_task_requires_list(Task :: map(), AllTaskNames :: sets:set()) -> {ok, map()} | {error, binary()}.
validate_task_requires_list(Task, AllTaskNames) ->
    case maps:get(<<"requires">>, Task, undefined) of
        undefined -> 
            {ok, Task};
        Requires when is_list(Requires) ->
            TaskName = maps:get(<<"name">>, Task),
            RequiresSet = sets:from_list(Requires),
            case {sets:is_subset(RequiresSet, AllTaskNames), sets:is_element(TaskName, RequiresSet)} of
                {true, false} ->
                    {ok, Task};
                _ ->
                    {error, error_message:payload_invalid_task_name_in_requires()}
            end;
        _ -> 
            {error, error_message:payload_invalid_task_requires()}
    end.