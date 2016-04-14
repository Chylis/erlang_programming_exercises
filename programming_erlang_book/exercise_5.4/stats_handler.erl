-module(stats_handler).
-export([init/1, terminate/1, handle_event/2]).

init(Stats) -> 
  Stats.

terminate(Stats) -> 
  {stats, Stats}.

handle_event({Type, _Id, Description}, Stats) -> 
  TypeList = proplists:get_value(Type, Stats, [{Description, 0}]),
  Count = proplists:get_value(Description, TypeList, 0) + 1,
  TypeList2 = lists:keystore(Description, 1, TypeList, {Description, Count}),
  lists:keystore(Type, 1, Stats, {Type, TypeList2});
handle_event(_Event, Stats) -> 
  Stats.

