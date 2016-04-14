-module(fsm).

idle() -> 
  receive
    {incoming, Number} -> 
      start_ringing(), 
      ringing(Number);
    off_hook -> 
      start_tone(),
      dial() 
  end.

ringing(Number) -> 
  receive
    {other_on_hook, Number} -> 
      stop_ringing(),
      idle();
    {off_hook, Number} ->
      stop_ringing(),
      connected(Number) 
  end.

dial() ->
  receive
    on_hook ->
      idle();
    {other_off_hook, Number} ->
      connected(Number)
  end.

connected(Number) ->
  receive
  on_hook ->
    idle();
end.

start_ringing() -> 
  event_manager:send_event(phone_fsm, {start_ringing, self(), now()}).

start_tone() -> 
  event_manager:send_event(phone_fsm, {start_tone, self(), now()}).

stop_ringing() ->
  event_manager:send_event(phone_fsm, {stop_ringing, self(), now()}).
