-module(tracer).
-export([init/0]).

%% starts the tracer. Here we instantiate the other algorithm traces. 
init() ->
    start_link().

%% the tracer pid receives messages, and then calls the various tracing mechanisms
% and then sends them back to the caller pid
start_link() ->
    TracePid = self(),
    receive
        {From, {reduce, Args}} ->
            %% implement reduce traces
            %% first get the number of processors
            Dict = dict:from_list(Args),
            Nprocs = dict:fetch(n_procs, Dict), 
            Results = reduce_trace:start(Nprocs),
            io:format("Trace Results: ~p ~n", [Results]),
            From ! {trace_data, Results},
            Results;
        _ -> invalid_request
    end,
    TracePid.