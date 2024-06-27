-module(job_processor_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Port = get_port(),
    erlang:display(Port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", router, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    job_processor_sup:start_link().

stop(_State) ->
    ok.

%% Private functions

get_port() ->
    case application:get_env(job_processor, port) of
        {ok, Port} when is_integer(Port) ->
            Port;
        _ ->
            io:format("Invalid port specified~n")
    end.