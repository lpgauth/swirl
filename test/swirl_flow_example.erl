-module(swirl_flow_example).
-include("../include/swirl.hrl").

-behavior(swirl_flow).
-export([
    map/3,
    reduce/3,
    output/4
]).

%% swirl_flow callbacks
map(StreamName, Event, _MapperOpts) ->
    {{?LM(type, Event), StreamName, ?LM(exchange_id, Event), ?LM(bidder_id, Event)}, {1, 10}}.

reduce(_Flow, Row, _ReducerOpts) ->
    Row.

output(_Flow, _Period, Rows, OutputOpts) ->
    case ?L(send_to , OutputOpts) of
        undefined -> ok;
        Pid -> Pid ! Rows
    end.
