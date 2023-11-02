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

    project_lib:create(
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

    % signal the end of the collection to the collector
    et_collector:report_event(CollectorPid, 80, watcher, watcher, "Event collection ended", [{action, null}, {value, V}]),
    % et_collector:save_event_file(CollectorPid, SaveFileName, [existing, write, keep]),
    % get data from the collector
    CollectorData = et_collector:iterate(CollectorPid, first, infinity, pre_json_encode_events(), []),

    %% return the path to the file to which all traces where stored
    lists:reverse(CollectorData).


%% ===============
%  Worker function
%% ===============

% Lin & Snyder style reduce:
%   called by the leaves.
%   returns the GrandTotal to each leaf.
reduce(ProcInfo, CombineFun, Value, CollectorPid) ->
    reduce(project_lib:parent_pid(ProcInfo), project_lib:child_pids(ProcInfo), CombineFun, Value, CollectorPid).

reduce({_}, [], _, GrandTotal, CollectorPid) -> 
    % signal that we are the top of the tree and and we are returning the last value
    et_collector:report_event(CollectorPid, 80, head, head, "At top of tree, returning grand total", [{action, null}, {value, GrandTotal}]),
    GrandTotal;
reduce(ParentPid, [], _, MyTotal, CollectorPid) ->
    % signal that we are at the leaves of each tree
    et_collector:report_event(CollectorPid, 80, ParentPid, self(), "At the leaf", [{action, null}, {value, MyTotal}]),
    ParentPid ! {self(), reduce_up, MyTotal},
    et_collector:report_event(CollectorPid, 80, self(), ParentPid, "At the leaf, sent data to parent", [{action, reduce_up}, {value, MyTotal}]),
    receive
        {ParentPid, reduce_down, GrandTotal} ->
            % signal that at the leaves we have received a value from our parent
            et_collector:report_event(CollectorPid, 80, ParentPid, self(), "At the leaf, receive from parent, grand total", [{action, reduce_down}, {value, GrandTotal}]),
            GrandTotal
    end;
reduce(Parent, [ChildHd | ChildTl], CombineFun, LeftTotal, CollectorPid) ->
   
    % signal the going down the tree
    et_collector:report_event(CollectorPid, 80, Parent, self(), "Inside tree, going down", [{action, null}, {value, LeftTotal}]),
    
    receive
        {ChildHd, reduce_up, RightTotal} ->
            % signal that we have received data from a child
            et_collector:report_event(CollectorPid, 80, ChildHd, self(), "At node, receive right total from child", [{action, null}, {value, RightTotal}]),
            GrandTotal = reduce(Parent, ChildTl, CombineFun, CombineFun(LeftTotal, RightTotal), CollectorPid),
            
            % signal that we have sent data to a child
            et_collector:report_event(CollectorPid, 80, self(), ChildHd, "At node, sent grand total to child", [{action, reduce_down}, {value, GrandTotal}]),
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

        {Type, Priority, _, _, From, To, Msg, Data} = Event,
        
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
            {<<"from">>, From},
            {<<"to">>, To},
            {<<"msg">>, Msg},
            {<<"data">>, Data}
        ],

        [Instance | Acc]
    end.