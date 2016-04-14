-module(echo).
-export([start/0, print/1, stop/0]).
-export([loop/0]).

start() ->
  case whereis(echo) of
    undefined ->
      register(echo, spawn(?MODULE, loop, [])),
      ok;
    _Pid ->
      ok
  end.

print(Term) ->
  echo ! {print, Term},
  ok.

stop() ->
  echo ! stop,
  ok.

loop() ->
  receive
    {print, Term} ->
      io:format("~p", [Term]),
      loop();
    stop ->
      ok
  end.
