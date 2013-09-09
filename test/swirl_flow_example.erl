-module(swirl_flow_example).
-include("../include/swirl.hrl").

-behavior(swirl_flow).
-export([
    map/3,
    reduce/3
]).

%% swirl_flow callbacks
map(StreamName, Event, _MapperOpts) ->
    {update, {StreamName, ?L(exchange_id, Event), ?L(bidder_id, Event)}, {1, 10}}.

reduce(_Period, Counters, ReducerOpts) ->
    case ?L(send_to , ReducerOpts) of
        undefined -> ok;
        Pid -> Pid ! Counters
    end.
