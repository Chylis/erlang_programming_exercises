-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1, get_frequencies/0]). 
-export([init/0]).

%% These are the start functions used to create and %% initialize the server.
start() ->
  register(frequency, spawn(frequency, init, [])).

init() ->
  Frequencies = {[10,11,12,13,14,15], []}, 
  loop(Frequencies).

%% The client Functions
get_frequencies() -> 
  {Free, Allocated} = call(get_frequencies),
  io:format("~n Free frequencies: ~p", [Free]),
  io:format("~n Allocated frequencies: ~p~n", [Allocated]).

stop() -> 
  call(stop).

allocate() -> 
  call(allocate). 

deallocate(Freq) -> 
  call({deallocate, Freq}).

%% We hide all message passing and the message protocol in a functional interface.
call(Message) ->
  frequency ! {request, self(), Message}, 
  receive {reply, Reply} -> Reply end.

%% The Main Loop
loop(Frequencies) -> 
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid), 
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} -> 
      NewFrequencies = deallocate(Frequencies, Freq, Pid), 
      reply(Pid, ok),
      loop(NewFrequencies);
    {request, Pid, get_frequencies} ->
      reply(Pid, Frequencies),
      loop(Frequencies);
    {request, Pid, stop} ->
      reply(Pid, ok) 
  end.

reply(Pid, Reply) -> 
  Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) -> 
  AllocatedByPid = [{F, P} || {F, P} <- Allocated, P == Pid],
  case length(AllocatedByPid) > 2 of 
    true ->
      {{[Freq|Free], Allocated}, {error, cap_reached}};
    false ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.



deallocate(Frequencies, Freq, Pid) -> 
  deallocate(Frequencies, Freq, Pid, []).

deallocate({Free, []}, _Freq, _Pid, NewAllocated) -> 
  {Free, lists:reverse(NewAllocated)};
deallocate({Free, [{Freq, Pid}|Allocated]}, Freq, Pid, Acc) -> 
  deallocate({[Freq|Free], Allocated}, Freq, Pid, Acc);
deallocate({Free, [H|Allocated]}, Freq, Pid, Acc) -> 
  deallocate({Free, Allocated}, Freq, Pid, [H|Acc]).
