-module(project_lib).
-include_lib("eunit/include/eunit.hrl").
-export([create/2, child_pids/1, parent_pid/1, proc_index/1, plus_fun/0]).

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
  spawn(fun() -> create(NProcs, {MyPid}, 1, [], Task) end).

create(1, Parent, MyIndex, ChildPids, Task) ->
  Task({Parent, ChildPids, MyIndex});
create(N, Parent, MyIndex, ChildPids, Task) when is_integer(N), 1 < N ->
  NLeft = N div 2,
  NRight = N - NLeft,
  MyPid = self(),
  RightPid = spawn(fun() ->
                    create(NRight, MyPid, MyIndex + NLeft, [], Task)
                   end),
  create(NLeft, Parent, MyIndex, [RightPid | ChildPids], Task).
