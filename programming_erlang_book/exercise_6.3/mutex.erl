-module(mutex). 
-export([start/0, stop/0]). 
-export([wait/0, signal/0]). 
-export([init/0]).


start() ->
  register(mutex, spawn_link(?MODULE, init, [])),
  {ok, whereis(mutex)}.

stop() -> 
  mutex ! stop.

init() -> 
  process_flag(trap_exit, true),
  free().

%%%
%%% Internal Mutex
%%%

free() -> 
  receive
    {wait, Pid} -> 
      try link(Pid) of
        true -> 
          Pid ! ok, 
          busy(Pid)
      catch error:noproc ->
        free()
      end;

    stop ->
      terminate() 
  end.

busy(Pid) -> 
  receive
    {'EXIT',Pid,_Reason} ->
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
