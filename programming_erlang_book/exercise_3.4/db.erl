-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
  [].

destroy(_Db) ->
  ok.

write(Key, Element, Db) ->
  Db2 = delete(Key, Db),
  [{Key, Element}|Db2].

delete(Key, Db) ->
  delete(Key, Db, []).

read(Key, Db) ->
  value_for_key(Key, Db).

match(Element, Db) ->
  keys_for_value(Element, Db, []).

%%%
%%% Private API
%%%

% Deletes the entry with key 'Key'
delete(_Key, [], Acc) ->
  reverse(Acc);
delete(Key, [{Key, _Elem}|T], Acc) ->
  delete(Key, T, Acc);
delete(Key, [H|T], Acc) ->
  delete(Key, T, [H|Acc]).

% Reverses the received List
reverse(List) ->
  reverse(List, []).
reverse([], Acc) ->
  Acc;
reverse([H|T], Acc) ->
  reverse(T, [H|Acc]).

% Returns the associated value for key 'Key', or {error, instance} if no such key exists
value_for_key(_Key, []) ->
  {error, instance};
value_for_key(Key, [{Key, Elem}|_T]) ->
  {ok, Elem};
value_for_key(Key, [_H|T]) ->
  value_for_key(Key, T).

% Returns a list containing all keys with value 'Val'
keys_for_value(_Val, [], Acc) ->
  Acc;
keys_for_value(Val, [{Key, Val}|T], Acc) ->
  keys_for_value(Val, T, [Key|Acc]);
keys_for_value(Val, [_H|T], Acc) ->
  keys_for_value(Val, T, Acc).
