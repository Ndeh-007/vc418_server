-module(scan_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, State) ->

    %% handling the get request for tracing the executions of a scan application
    %% the request handler assumes all requests coming in are get request and handled accordingly. 
    %% All error handling on checking what kind of data should be sent is done on the client side
    %%  

    #{nprocs := NProcs} = cowboy_req:match_qs([{nprocs, int}], Req),

    Response = scan_trace:start(NProcs),
    JsonResponse = jsone:encode(Response),
    Req_1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"application/json">>},
        JsonResponse,
        Req
    ),
    {ok, Req_1, State}.