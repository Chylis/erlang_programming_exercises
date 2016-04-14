-module(binary_tree).
-export([build_ordered_tree/0, build_unordered_tree/0, insert/2, sum/1, find_min/1,find_max/1, is_ordered/1]).
-record(bt,{left,value,right}).

sum(#bt{left=undefined,value=V,right=undefined}) ->
  V;
sum(#bt{left=L,value=V,right=R}) ->
  sum(L) + V + sum(R);
sum(_) ->
  0.

find_max(#bt{value=V}=BT) -> find_max(BT,V).
find_max(#bt{left=L,value=V,right=R}, Max) -> max(max(V,max(find_max(L,Max),find_max(R,Max))),Max); % HAHA
find_max(_, Max) -> Max.

find_min(#bt{value=V}=BT) -> 
  find_min(BT,V).
find_min(#bt{left=L,value=V,right=R}, Min) -> 
  ChildMin = min(find_min(L,Min), find_min(R,Min)),
  min(Min, min(V, ChildMin));
find_min(_, Min) -> Min.


is_ordered(BT) when is_record(BT, bt) ->
  is_ordered(BT,-99999,99999).
is_ordered(undefined,_,_) ->
  true;
is_ordered(#bt{value=V},Min,Max) when V < Min orelse V > Max ->
  false;
is_ordered(#bt{left=L,value=V,right=R}, Min, Max) ->
  is_ordered(L,Min,V-1) andalso is_ordered(R,V+1,Max).

insert(Val, #bt{left=L,value=V} = BT) when Val < V ->
  BT#bt{left=insert(Val,L)};
insert(Val, #bt{right=R,value=V} = BT) when Val > V ->
  BT#bt{right=insert(Val,R)};
insert(Val, _) ->
  #bt{value=Val}.






%%%
%%% Test
%%%

build_ordered_tree() ->
  One=#bt{value=1},
  Five=#bt{value=5},
  Four=#bt{value=4,right=Five,left=One},
  Two=#bt{value=2},
  _Root=#bt{left=Two,right=Four,value=3}.

build_unordered_tree() ->
  Three=#bt{value=3},
  Five=#bt{value=5},
  Four=#bt{value=4,left=Three},
  Two=#bt{value=2,right=Four},
  _Root=#bt{left=Two,right=Five,value=3}.
