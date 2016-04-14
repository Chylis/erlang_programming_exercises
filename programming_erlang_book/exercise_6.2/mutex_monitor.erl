-module(mutex_monitor). 
-export([start/0, stop/0]). 
-export([wait/0, signal/0]). 
-export([init/0]).


start() ->
  register(mutex, spawn(?MODULE, init, [])).

stop() -> 
  mutex ! stop.

init() -> 
  free().

%%%
%%% Internal Mutex
%%%

free() -> 
  receive
    {wait, Pid} -> 
      monitor(process, Pid),
      Pid ! ok, 
      busy(Pid);
    stop ->
      terminate() 
  end.

busy(Pid) -> 
  receive
    {'DOWN',Ref,process,Pid,_Reason} ->
      demonitor(Ref),
      free();
    {signal, Pid} -> 
      free()
  end.

terminate() -> 
  receive
    {wait, Pid} -> 
      exit(Pid, kill), 
      terminate()
  after
    0 -> ok
  end.

%%%
%%% Client API
%%%

wait() ->
  mutex ! {wait, self()}, 
  receive ok -> 
      ok 
  end.

signal() ->
  mutex ! {signal, self()}, 
  ok.
