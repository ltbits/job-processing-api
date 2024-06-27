-module(task_sorter).
-export([sort/1]).

%% @doc Sorts tasks based on their dependencies.
%% Returns a sorted list of tasks or false if sorting is impossible.
-spec sort(list(map())) -> list(map()) | false.
sort(Tasks) ->
    G = digraph:new(),
    try
        add_tasks(Tasks, G),
        case digraph_utils:topsort(G) of
            false ->
                false;
            SortedNames ->
                TaskMap = create_task_map(Tasks),
                [maps:get(Name, TaskMap) || Name <- SortedNames]
        end
    catch
        throw:cycle -> 
            false
    after
        digraph:delete(G)
    end.

-spec add_tasks(list(map()), digraph:graph()) -> ok.
add_tasks([], _G) ->
    ok;
add_tasks([Task | Rest], G) ->
    Name = maps:get(<<"name">>, Task),
    digraph:add_vertex(G, Name, Task),
    Deps = maps:get(<<"requires">>, Task, []),
    add_edges(Name, Deps, G),
    add_tasks(Rest, G).

-spec add_edges(binary(), list(binary()), digraph:graph()) -> ok.
add_edges(_, [], _G) ->
    ok;
add_edges(Name, [Dep | Rest], G) ->
    digraph:add_vertex(G, Dep),
    case digraph:add_edge(G, Dep, Name) of
        {error, {bad_edge, _}} -> throw(cycle);
        _ -> add_edges(Name, Rest, G)
    end.

-spec create_task_map(list(map())) -> map().
create_task_map(Tasks) ->
    maps:from_list([{maps:get(<<"name">>, Task), Task} || Task <- Tasks]).