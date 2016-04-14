-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0]).

start() ->
  case is_started() of 
    true ->
      ok;
    false ->
      register(my_db, spawn(?MODULE, init, []))
  end.

stop() ->
  case is_started() of 
    true ->
      send(stop);
    false ->
      ok
  end.

write(Key, Element) ->
  send({write, Key, Element}).

delete(Key) ->
  send({delete, Key}).

read(Key) ->
  send({read, Key}).

match(Element) ->
  send({match, Element}).

%%%
%%% Private API
%%%

is_started() ->
  whereis(my_db) =/= undefined.

send(Message) ->
  my_db ! {request, self(), Message},
  receive {reply, Reply} -> Reply end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

init() ->
  loop(db:new()).

loop(Db) ->
  receive
    {request, Pid, {write, Key, Element}} ->
      Db2 = db:write(Key, Element, Db),
      reply(Pid, ok),
      loop(Db2);
    {request, Pid, {delete, Key}} ->
      Db2 = db:delete(Key, Db),
      reply(Pid, ok),
      loop(Db2);
    {request, Pid, {read, Key}} ->
      reply(Pid, db:read(Key, Db)),
      loop(Db);
    {request, Pid, {match, Element}} ->
      reply(Pid, db:match(Element, Db)),
      loop(Db);
    {request, Pid, stop} ->
      reply(Pid, db:destroy(Db))
  end.
