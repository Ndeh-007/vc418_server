%%%-------------------------------------------------------------------
%% @doc vc418_server public API
%% @end
%%%-------------------------------------------------------------------

-module(vc418_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
                { <<"/">>, hello_handler, [] },
                { <<"/reduce">>, reduce_handler,[]},
                { <<"/scan">>, scan_handler,[]}
            ]
        } 
    ]),
    {ok, _} = cowboy:start_clear(
        listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    vc418_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
