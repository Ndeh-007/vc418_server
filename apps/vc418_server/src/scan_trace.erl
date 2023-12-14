-module(scan_trace).
-include_lib("eunit/include/eunit.hrl").
-export([start/1]).


start(NProcs) ->
    % create the worker process and instantiate them with the scan using the the
    % textbook Lin and Snyder

    % instantiate the collector
    CollectorOptions = [
        {parent_pid, self()}, {event_order, trace_ts},
        {trace_global, false}, {trace_pattern, undefined},
        {trace_port, 4711}, {trace_max_queue, 100}
    ],
    {ok, CollectorPid} = et_collector:start_link(CollectorOptions),

    ConcatFun = fun(X, Y) -> X ++ Y end,

    % create processes and instantiate the scan.
    {RootPid, Tree} = project_lib:create(NProcs,
      fun(ProcInfo) ->
        I = scan(ProcInfo, plus_fun(), 0, 1, CollectorPid),
        project_lib:swap_data_with_master(ProcInfo, I),
        J = scan(ProcInfo, ConcatFun, [], [I], CollectorPid),
        project_lib:swap_data_with_master(ProcInfo, J)
      end),
    V = lists:seq(0, NProcs - 1),

    % test the scan
    ?assertEqual(V, project_lib:swap_data_with_tree(RootPid, [ok || _ <- V])),
    
    % get data from the collector
    CollectorData = et_collector:iterate(CollectorPid, first, infinity, pre_json_encode_events(), []),

    %% return the path to the file to which all traces where stored
    Events = lists:reverse(CollectorData),
    EncodedTree = project_lib:encoded_json_tree(Tree, []),
    {[ 
        {<<"nprocs">>, NProcs},
        {<<"program">>, list_to_binary("scan")},
        {<<"tree">>, EncodedTree},
        {<<"data">>, Events}
    ]}.



%% ===============
%  Worker function
%% ===============
scan(ProcInfo, CombineFun, AccIn, Value, CollectorPid) ->
    scan(project_lib:parent_pid(ProcInfo), project_lib:child_pids(ProcInfo), CombineFun, AccIn, Value, CollectorPid).

scan({_}, [], _, AccIn, _, _) -> 
    AccIn;
scan(ParentPid, [], _, _, MyTotal, CollectorPid) ->
    et_collector:report_event(CollectorPid, 80, ParentPid, self(), "At the leaf", [{action, null}, {left_total, MyTotal}, {my_total, MyTotal}, {acc, null}, {step_value, MyTotal}, {value, MyTotal}]),
    ParentPid ! {self(), send_up, MyTotal},
    receive 
        {ParentPid, send_down, GrandTotal} ->
            et_collector:report_event(CollectorPid, 80, self(), ParentPid, "At the leaf, received data from parent with action", [{action, send_down}, {left_total, MyTotal}, {my_total, MyTotal}, {acc, GrandTotal},  {step_value, GrandTotal}, {value, GrandTotal}]),
            GrandTotal
    end;
scan(Parent, [ChildHd | ChildTl], CombineFun, AccIn, LeftTotal, CollectorPid) ->
    receive
        {ChildHd, send_up, RightTotal} -> 
            et_collector:report_event(CollectorPid, 80, ChildHd, self(), "Inside tree received data", [{action, send_up}, {left_total, LeftTotal}, {my_total, RightTotal}, {acc, AccIn}, {step_value, CombineFun(LeftTotal, RightTotal)}, {value, LeftTotal}]),
            GrandTotal = scan(Parent, ChildTl, CombineFun, AccIn, CombineFun(LeftTotal,RightTotal), CollectorPid),
            ChildHd ! {self(), send_down, CombineFun(LeftTotal, GrandTotal)},
            GrandTotal
    end.
   
 
    
%% ===============
% Helper functions
%% ==============


% Add Function
plus_fun() -> 
    fun(X, Y) -> X + Y end.

%%
% helper function for getting the data ready for encoding in and sending the request
% 
pre_json_encode_events() -> 
    fun(Event, Acc) ->

        {Type, Priority, STime, RTime, From, To, Msg, Data} = Event,
        
        % create a json encoded instance
        ST = project_lib:get_as_milliseconds(STime),
        RT = project_lib:get_as_milliseconds(RTime),

        Instance = [
            {<<"type">>, Type},
            {<<"priority">>, Priority},
            {<<"send_time">>, ST},
            {<<"receive_time">>, RT},
            {<<"from">>, From},
            {<<"to">>, To},
            {<<"msg">>, list_to_binary(Msg)},
            {<<"data">>, Data}
        ],

        [Instance | Acc]
    end.