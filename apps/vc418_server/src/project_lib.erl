-module(project_lib).

-export([create/2, child_pids/1, parent_pid/1, proc_index/1, plus_fun/0, get_as_milliseconds/1, encoded_json_tree/2]).
-export([swap_data_with_master/2, swap_data_with_tree/2]).
-export([close/2]).

-include_lib("eunit/include/eunit.hrl").

%% helper functions
plus_fun() -> fun(X, Y) -> X + Y end.


% Each process in the process-trees we create has a ProcInfo object.
% Currently, this is a tuple with three fields:
%   {ParentPid, ChildPids, Index}
% If this node is the root of the process tree, then ParentPid is a tuple
% of the form {MasterPid} where MasterPid is the pid of the process that
% created this tree.
% This structure might change as we add more functionality to process trees.
% Thus, ProcInfo objects should only be created by create and only accessed
% by the functions below:
child_pids({_, CPids, _}) -> CPids.
parent_pid({PPid, _, _}) -> PPid.
proc_index({_, _, I}) -> I.

% create(NProcs, Task) -> RootPid
%   RootPid is the pid for the process at the root of the tree.
create(NProcs, Task)
  when is_integer(NProcs), 0 < NProcs, is_function(Task, 1) ->
  MyPid = self(),
  MasterPid = self(),
  RootPid = spawn(fun() -> create(NProcs, {MyPid}, 1, [], Task, MasterPid) end),
  ProcTree = loop_create([], NProcs),
  {RootPid, ProcTree}.

create(1, Parent, MyIndex, ChildPids, Task, MasterPid) ->
  MasterPid ! {proc_state, {Parent, self(), ChildPids, MyIndex, MyIndex}},
  Task({Parent, ChildPids, MyIndex});
create(N, Parent, MyIndex, ChildPids, Task, MasterPid) when is_integer(N), 1 < N ->
  NLeft = N div 2,
  NRight = N - NLeft,
  MyPid = self(),
  RightPid = spawn(fun() ->
                    create(NRight, MyPid, MyIndex + NLeft, [], Task, MasterPid)
                   end),
  create(NLeft, Parent, MyIndex, [RightPid | ChildPids], Task, MasterPid).

loop_create(Acc, NProcs) when length(Acc) == NProcs -> Acc; 
loop_create(Acc, NProcs) ->
  receive
    {proc_state, ProcInfo} -> 
      loop_create([ProcInfo | Acc], NProcs) 
  end.

encoded_json_tree([], Acc) -> Acc;
encoded_json_tree([{Parent, Self, Children, Index, Value} | TreeListTl], Acc)->
  Instance = [
    {<<"self">>, Self},
    {<<"parent">>, Parent},
    {<<"children">>, Children},
    {<<"value">>, Value},
    {<<"index">>, Index}
  ],
  encoded_json_tree(TreeListTl, [Instance | Acc]).

% swap_data_with_master(ProcInfo, UpData) -> DownData
%   called by worker processes.
%   Parameters:
%     ProcInfo is the ProcInfo object for the process (doh!)
%     UpData is our data to send to the master process.
%   Return Value:
%     DownData: our data from the master process.
%   Note: the master process will receive a list of UpData values, with
%     one element per worker process, in order of their process indices.
%     The master process provides a list of new data values, with one
%     element per worker process.  Each worker receives the DownData value
%     corresponding to its position in this list.
swap_data_with_master(ProcInfo, UpData) ->
  swap_data_with_master(parent_pid(ProcInfo), child_pids(ProcInfo), {UpData}).

swap_data_with_master({MasterPid}, [], UpData) ->
  MasterPid ! {self(), data_up, UpData},
  receive
    {MasterPid, data_down, DownData} -> DownData
  end;
swap_data_with_master(ParentPid, [], UpData) ->
  ParentPid ! {self(), data_up, UpData},
  receive
    {ParentPid, data_down, DownData} -> DownData
  end;
swap_data_with_master(Parent, [ChildHd | ChildTl], LeftUp) ->
  receive
    {ChildHd, data_up, RightUp} ->
      [LeftDown, RightDown] =
        swap_data_with_master(Parent, ChildTl, [LeftUp, RightUp]),
      ChildHd ! {self(), data_down, RightDown},
      LeftDown
  end.

swap_data_with_tree(RootPid, ListDown) ->
  receive
    {RootPid, data_up, DataUp} ->
      try
        case swap_magic(DataUp, [], ListDown) of
          {DataDown, ListUpRev, []} ->
            RootPid ! {self(), data_down, DataDown},
            lists:reverse(ListUpRev);
          _ -> io:format("swap_data_with_tree: too many values in ListDown" ++
          "  length(ListDown) = ~w, should be ~w~n",
            [length(ListDown), length(lists:flatten(DataUp))]),
            error(bad_arg)
        end
      catch error:not_enough_data ->
        io:format("swap_data_with_tree: not enough values in ListDown" ++
        "  length(ListDown) = ~w, should be ~w~n",
          [length(ListDown), length(lists:flatten(DataUp))]),
        error(bad_arg)
      end
  end.

swap_magic({Leaf1}, Rev1, [Hd2 | Tl2]) ->
  {Hd2, [Leaf1 | Rev1], Tl2};
swap_magic(_, _, []) ->
  error(not_enough_data);
swap_magic([L1, R1], Rev1, List2) ->
  {NewL1, Rev1a, List2a} = swap_magic(L1, Rev1, List2),
  {NewR1, Rev1b, List2b} = swap_magic(R1, Rev1a, List2a),
  {[NewL1, NewR1], Rev1b, List2b}.

close(X, X) -> true;
close(Expected, Actual) when is_number(Expected), is_number(Actual) ->
  abs(Actual - Expected) < 1.0e-8 * max(abs(Expected), 1);
close([HdExpected | TlExpected], [HdActual | TlActual]) ->
  close(HdExpected, HdActual) andalso
    close(TlExpected, TlActual);
close(_, _) -> false.


get_as_milliseconds({MegaSecs, Secs, MicroSecs}) ->
  MegaSecs * 1000000 * 1000 + Secs * 1000 + MicroSecs.