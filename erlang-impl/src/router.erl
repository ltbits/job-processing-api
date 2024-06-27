-module(router).
-export([init/2, terminate/3]).

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req0, State) ->
    try
        handle_request(Req0, State)
    catch
        error:Error:Stacktrace ->
            logger:error("Request failed: ~p~nStacktrace: ~p", [Error, Stacktrace]),
            {Status, ErrorResp} = controller:generic_error_response(),
            Req = response(Req0, Status, ErrorResp),
            {ok, Req, State}
    end.

-spec handle_request(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
handle_request(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {ok, Data, Req} = cowboy_req:read_body(Req0), % Assumed payload is within 8MB limit.
    case {Method, Path} of
        {<<"POST">>, <<"/process">>} ->
            {Status, Body, ContentType} = controller:process_tasks(json:decode(Data)),
            Req1 = response(Req, Status, Body, ContentType),
            {ok, Req1, State};
        _ ->
            Req1 = response(Req, 404, #{error => <<"Not found">>}),
            {ok, Req1, State}
    end.

-spec response(cowboy_req:req(), integer(), any()) -> cowboy_req:req().
response(Req, Status, Body) ->
    response(Req, Status, Body, json).

-spec response(cowboy_req:req(), integer(), any(), json | text) -> cowboy_req:req().
response(Req, Status, Body, ContentType) ->
    {ResContentType, ResBody} = case ContentType of
        text -> {<<"text/plain">>, Body};
        json -> {<<"application/json">>, json:encode(Body)}
    end,
    cowboy_req:reply(Status, 
        #{<<"content-type">> => <<ResContentType/binary, "; charset=utf-8">>}, 
        ResBody, 
        Req).

-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.