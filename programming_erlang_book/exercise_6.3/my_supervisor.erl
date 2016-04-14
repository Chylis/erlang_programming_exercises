-module(my_supervisor). 
-export([start_link/2, stop/1, start_child/5, stop_child/2, view_children/1]). 
-export([init/1]).

-define(MAX_NUMBER_OF_RESPAWN_ATTEMPTS, 5).
-define(RESPAWN_ATTEMPT_SECONDS_INTERVAL, 60).

%%% 
%%% Client API
%%%

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(?MODULE, init, [ChildSpecList])), 
  ok.

stop(Name) ->
  Name ! {stop, self()},
  receive 
    {reply, Reply} -> Reply 
  end.

start_child(Name, Module, Function, Args, Type) ->
  Name ! {start_child, self(), {Module,Function,Args,Type}},
  receive Reply -> Reply end.

stop_child(Name, Id) ->
  Name ! {stop_child, Id},
  ok.

view_children(Name) ->
  Name ! view_children,
  ok.


%%% 
%%% Internal
%%%

init(ChildSpecList) -> 
  process_flag(trap_exit, true), 
  loop(start_children(ChildSpecList)).

start_children([]) -> 
  [];
start_children([{M, F, A, Type} | ChildSpecList]) ->
  % It is the responsibility of the children to link themselves to the calling/parent process and to return {ok, Pid}
  case start_child(M,F,A,Type) of
    error ->
      start_children(ChildSpecList);
    Child ->
      [Child|start_children(ChildSpecList)] 
  end.

start_child(Module, Function, Args, Type) ->
  case (catch apply(Module,Function,Args)) of 
    {ok, Pid} ->
      {{id, make_ref()}, {pid, Pid}, {child_info, Module,Function,Args,Type}, {spawn_info, calendar:local_time(), 1}};
    _ ->
      error
  end.

loop(ChildList) -> 
  receive
    {'EXIT', Pid, Reason} ->
      {value, DeadChild} = lists:keysearch({pid, Pid}, 2, ChildList),
      NewChildList = restart_child(DeadChild, Reason, ChildList), 
      loop(NewChildList);
    {start_child, From, {M,F,A,T}} ->
      % Todo: Handle erroneous case of start_child
      Child = {{id, Id},{pid, _},{child_info, _,_,_,_},{spawn_info, _,_}} = start_child(M,F,A,T),
      From ! {ok, Id},
      NewChildList = [Child|ChildList],
      loop(NewChildList);
    {stop_child, Id} ->
      loop(delete_child({id, Id}, ChildList));
    view_children ->
      io:format("~n~p~n", [ChildList]),
      loop(ChildList);
    {stop, From} ->
      From ! {reply, terminate(ChildList)} 
  end.

delete_child({id, Id}, ChildList) ->
  lists:keydelete({id,Id},1,ChildList);
delete_child({pid, Pid}, ChildList) ->
  lists:keydelete({pid,Pid},2,ChildList).


restart_child({{id, _Id},{pid, Pid}, {child_info, M,F,_A,transient}, {spawn_info, _SpawnTime, _SpawnCount}}, normal, ChildList) ->
  io:format("Transient child '~p:~p' terminated normally - not restarting~n", [M, F]),
  delete_child({pid,Pid}, ChildList);
restart_child({{id, Id}, {pid, Pid}, {child_info, M,F,A,Type}, {spawn_info, SpawnTime, SpawnCount}}, Reason, ChildList) when SpawnCount >= ?MAX_NUMBER_OF_RESPAWN_ATTEMPTS ->
  NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  SpawnTimeSeconds = calendar:datetime_to_gregorian_seconds(SpawnTime),
  ExceedsRespawnInterval = NowSeconds - SpawnTimeSeconds =< ?RESPAWN_ATTEMPT_SECONDS_INTERVAL,
  case ExceedsRespawnInterval of
    true ->
      io:format("~p child '~p:~p' has a respawn count of: ~p within the last minute - not restarting~n", [Type, M, F, SpawnCount]),
      delete_child({pid, Pid}, ChildList);
    false ->
      Child = {{id, Id}, {pid, Pid}, {child_info, M,F,A,Type},{spawn_info, SpawnTime, 1}},
      restart_child(Child, Reason, ChildList)
  end;
restart_child({{id, Id}, {pid, Pid}, {child_info, M,F,A,Type}, {spawn_info,_SpawnTime, SpawnCount}}, Reason, ChildList) ->
  io:format("~p child '~p:~p' terminated with reason: ~p - restarting~n", [Type, M, F, Reason]),

  % Unhandled case: if apply throws exception
  {ok, NewPid} = apply(M,F,A), 

  Child = {{id,Id},{pid,NewPid},{child_info,M,F,A,Type}, {spawn_info,calendar:local_time(), SpawnCount+1}},
  [Child|delete_child({pid,Pid},ChildList)].


terminate([{Pid, _} | ChildList]) -> 
  exit(Pid, kill), 
  terminate(ChildList);
terminate(_ChildList) -> 
  ok.
