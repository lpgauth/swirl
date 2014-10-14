-module(swirl_flow_example).

-behavior(swirl_flow).
-export([
    map/3,
    reduce/3,
    output/4
]).

%% swirl_flow callbacks
map(StreamName, Event, _MapperOpts) ->
    {{lm(type, Event), StreamName, lm(exchange_id, Event), lm(bidder_id, Event)}, {1, 10}}.

reduce(_Flow, Row, _ReducerOpts) ->
    Row.

output(_Flow, _Period, Rows, OutputOpts) ->
    case l(send_to , OutputOpts) of
        undefined -> ok;
        Pid -> Pid ! Rows
    end.

%% helpers
l(Key, Event) ->
    swirl_utils:lookup(Key, Event, undefined).

lm(Key, Event) ->
    maps:get(Key, Event, undefined).
