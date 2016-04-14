-module(distributed_ring).
-export([start/3]).
-export([init/3, create_neighbour/2, loop/1]).

start(M, N, Message) ->
  spawn(?MODULE, init, [M, N-1, Message]).

init(M, N, Message) ->
  % Create self-spawning offspring
  Neighbour = spawn(?MODULE, create_neighbour, [N-1, self()]),

  % Send first message around the ring
  send_message(M, Message, Neighbour),

  % Listen for incoming
  loop(Neighbour).

create_neighbour(N, InitialPid) when N > 0 ->
  Neighbour = spawn(?MODULE, create_neighbour, [N-1, InitialPid]),
  loop(Neighbour);
create_neighbour(_N, InitialPid) ->
  loop(InitialPid).

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
