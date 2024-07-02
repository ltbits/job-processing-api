-module(task_sorter).
-export([sort/1]).

%% @doc Sorts the given list of tasks based on their dependencies.
-spec sort(Tasks :: [map()]) -> {ok, [map()]} | {error, binary()}.
sort(Tasks) ->
    G = digraph:new(),
    try
        add_tasks(Tasks, G),
        case digraph_utils:topsort(G) of
            false ->
                {error, error_message:circular_dependency()};
            SortedNames ->
                TaskMap = create_task_map(Tasks),
                {ok, [maps:get(Name, TaskMap) || Name <- SortedNames]}
        end
    catch
        throw:cycle -> 
            {error, error_message:circular_dependency()}
    after
        digraph:delete(G)
    end.

-spec add_tasks(Tasks :: [map()], G :: digraph:graph()) -> ok.
add_tasks([], _G) ->
    ok;
add_tasks([Task | Rest], G) ->
    Name = maps:get(<<"name">>, Task),
    digraph:add_vertex(G, Name, Task),
    Deps = maps:get(<<"requires">>, Task, []),
    add_edges(Name, Deps, G),
    add_tasks(Rest, G).

-spec add_edges(Name :: binary(), Deps :: [binary()], G :: digraph:graph()) -> ok.
add_edges(_, [], _G) ->
    ok;
add_edges(Name, [Dep | Rest], G) ->
    digraph:add_vertex(G, Dep),
    case digraph:add_edge(G, Dep, Name) of
        {error, {bad_edge, _}} -> throw(cycle);
        _ -> add_edges(Name, Rest, G)
    end.

-spec create_task_map(Tasks :: [map()]) -> #{binary() => map()}.
create_task_map(Tasks) ->
    maps:from_list([{maps:get(<<"name">>, Task), Task} || Task <- Tasks]).