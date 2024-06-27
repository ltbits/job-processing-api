-module(payload_validator).
-export([validate_input/0, validate_input/1]).

%% @doc Validates the input payload.
-spec validate_input(map()) -> boolean().
validate_input(#{<<"tasks">> := Tasks} = Input) when is_list(Tasks) ->
    validate_tasks(Tasks) andalso
    validate_optional_root_fields(Input) andalso
    validate_required_tasks(Tasks);
validate_input(_) -> false.

%% @doc Validates an empty input (always returns false).
-spec validate_input() -> boolean().
validate_input() -> false.

%% Private functions

-spec validate_tasks([map()]) -> boolean().
validate_tasks(Tasks) ->
    lists:all(fun validate_task/1, Tasks).

-spec validate_optional_root_fields(map()) -> boolean().
validate_optional_root_fields(Input) ->
    lists:all(fun(Field) -> validate_root_field(Input, Field) end, [<<"response_type">>]).

-spec validate_root_field(map(), binary()) -> boolean().
validate_root_field(Input, <<"response_type">>) ->
    case maps:get(<<"response_type">>, Input, undefined) of
        Value when is_binary(Value); Value =:= undefined -> true;
        _ -> false
    end;
validate_root_field(_, _) -> true.

-spec validate_task(map()) -> boolean().
validate_task(#{<<"command">> := Command, <<"name">> := Name} = Task) 
  when is_binary(Command), is_binary(Name) ->
    case maps:get(<<"requires">>, Task, undefined) of
        undefined -> true;
        Requires when is_list(Requires) -> true;
        _ -> false
    end;
validate_task(_) -> false.

-spec validate_required_tasks([map()]) -> boolean().
validate_required_tasks(Tasks) ->
    TaskNames = sets:from_list([maps:get(<<"name">>, Task) || Task <- Tasks]),
    lists:all(fun(Task) ->
        Name = maps:get(<<"name">>, Task),
        Requires = maps:get(<<"requires">>, Task, undefined),
        validate_task_requires(Requires, Name, TaskNames)
    end, Tasks).

-spec validate_task_requires(undefined | [binary()], binary(), sets:set()) -> boolean().
validate_task_requires(undefined, _, _) -> true;
validate_task_requires(Requires, TaskName, AllTaskNames) when is_list(Requires) ->
    RequiresSet = sets:from_list(Requires),
    not sets:is_element(TaskName, RequiresSet) andalso
    sets:is_subset(RequiresSet, AllTaskNames);
validate_task_requires(_, _, _) -> false.