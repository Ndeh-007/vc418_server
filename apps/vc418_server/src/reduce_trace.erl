-module(reduce_trace).
-include_lib("eunit/include/eunit.hrl").
-export([start/1]).

%% Defines the customized reduce function with tracers woven into the function.
% This basic function takes in the tracer collector process id and the the number of process
% Subsequent versions will also make accommodations for the initial data

start(NProcs) ->
    % create the worker process and instantiate them with the reduce using the the
    % textbook Lin and Snyder.

    MasterPid = self(),

    % instantiate the collector
    CollectorOptions = [
        {parent_pid, self()}, {event_order, trace_ts},
        {trace_global, false}, {trace_pattern, undefined},
        {trace_port, 4711}, {trace_max_queue, 100}
    ],
    {ok, CollectorPid} = et_collector:start_link(CollectorOptions),

    {_, Tree} = project_lib:create(
        NProcs,
        fun(ProcInfo) -> 
            I = project_lib:proc_index(ProcInfo),
            Total = reduce(ProcInfo, plus_fun(), I, CollectorPid),
            case I of
                1 -> MasterPid ! {self(), reduce_test, Total};
                _ -> ok
            end
        end
        ),
    V = receive
            {Pid, reduce_test, Total} when is_pid(Pid) -> Total
            after 1000 -> time_out
        end,
    
    ?assertEqual(lists:sum(lists:seq(1, NProcs)), V),

    % get data from the collector
    CollectorData = et_collector:iterate(CollectorPid, first, infinity, pre_json_encode_events(), []),

    %% return the path to the file to which all traces where stored
    Events = lists:reverse(CollectorData),
    EncodedTree = project_lib:encoded_json_tree(Tree, []),
    {[ 
        {<<"nprocs">>, NProcs},
        {<<"program">>, list_to_binary("reduce")},
        {<<"tree">>, EncodedTree},
        {<<"data">>, Events}
    ]}.


%% ===============
%  Worker function
%% ===============

% Lin & Snyder style reduce:
%   called by the leaves.
%   returns the GrandTotal to each leaf.
reduce(ProcInfo, CombineFun, Value, CollectorPid) ->
    reduce(project_lib:parent_pid(ProcInfo), project_lib:child_pids(ProcInfo), CombineFun, Value, CollectorPid).

reduce({_}, [], _, GrandTotal, _) -> 
    GrandTotal;
reduce(ParentPid, [], _, MyTotal, CollectorPid) ->
    % signal that we are at the leaves of each tree
    et_collector:report_event(CollectorPid, 80, ParentPid, self(), "At the leaf", [{action, null}, {value, MyTotal}, {step_value, MyTotal}]),
    ParentPid ! {self(), reduce_up, MyTotal},
    receive
        {ParentPid, reduce_down, GrandTotal} ->
            % signal that at the leaves we have received a value from our parent
            et_collector:report_event(CollectorPid, 80, ParentPid, self(), "At the leaf, receive from parent, grand total", [{action, reduce_down}, {value, GrandTotal}, {step_value, GrandTotal}]),
            GrandTotal
    end;
reduce(Parent, [ChildHd | ChildTl], CombineFun, LeftTotal, CollectorPid) ->
    
    receive
        {ChildHd, reduce_up, RightTotal} ->
            % signal that we have received data from a child
            et_collector:report_event(CollectorPid, 80, ChildHd, self(), "At node, receive right total from child", [{action, null}, {value, RightTotal}, {step_value, CombineFun(LeftTotal, RightTotal)}]),
            GrandTotal = reduce(Parent, ChildTl, CombineFun, CombineFun(LeftTotal, RightTotal), CollectorPid),
            
            % signal that we have sent data to a child
            ChildHd ! {self(), reduce_down, GrandTotal},
            
            GrandTotal
    end.

    
%% ===============
% Helper functions
%% ==============


% Add Function
plus_fun() -> fun(X, Y) -> X + Y end.

%%
% helper function for getting the data ready for encoding in and sending the request
% 
pre_json_encode_events() -> 
    fun(Event, Acc) ->

        {Type, Priority, STime, RTime, From, To, Msg, Data} = Event,
        
        %% Type => atom
        %% Priority => integer
        %% From => atom
        %% To => atom
        %% Msg => string
        %% Data => List of tuples: [..., {Key, Value}, ...]
        %%          where Key => atom
        %%                Value => integer or atom

        % create a json encoded instance

        Instance = [
            {<<"type">>, Type},
            {<<"priority">>, Priority},
            {<<"send_time">>, project_lib:get_as_milliseconds(STime)},
            {<<"receive_time">>, project_lib:get_as_milliseconds(RTime)},
            {<<"from">>, From},
            {<<"to">>, To},
            {<<"msg">>, list_to_binary(Msg)},
            {<<"data">>, Data}
        ],

        [Instance | Acc]
    end.