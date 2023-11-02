-module(reduce_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, State) ->
    Response = reduce_trace:start(8),
    JsonResponse = jsone:encode(Response),
    Req_1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"application/json">>},
        JsonResponse,
        Req
    ),
    {ok, Req, State}.