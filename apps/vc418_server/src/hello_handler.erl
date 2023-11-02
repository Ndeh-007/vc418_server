-module(hello_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, State) ->
    Req_1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"hello,world">>,
        Req
    ),
    {ok, Req, State}.