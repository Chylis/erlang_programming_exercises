-module(central_ring).
-export([start/3]).
-export([init/3, loop/1]).

start(M, N, Message) ->
  spawn(?MODULE, init, [M, N-1, Message]).

init(M, N, Message) ->
  % Spawn N processes in a ring
  Neighbour = spawn_process(N),

  % Send first message around the ring
  send_message(M, Message, Neighbour),

  % Listen for incoming
  loop(Neighbour).

spawn_process(N) ->
  spawn_process(N, self()).
spawn_process(N, Pid) when N > 0 ->
  spawn_process(N-1, spawn(?MODULE, loop, [Pid]));
spawn_process(_N, Pid) ->
  Pid.

send_message(M, Message, Pid) ->
  Pid ! {message, M, Message}.

loop(Neighbour) ->
  receive
    {message, M, Message} when M > 0 ->
      io:format("~p received message ~p~n", [self(), Message]),
      send_message(M-1, Message, Neighbour),
      loop(Neighbour);
    quit ->
      % Terminate gracefully when receive a quit message
      io:format("~p is quitting~n", [self()]),
      Neighbour ! quit, 
      ok;
    _Other -> 
      loop(Neighbour)
  end.
