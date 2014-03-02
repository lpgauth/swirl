-module(swirl_flow_example).
-include("../include/swirl.hrl").

-behavior(swirl_flow).
-export([
    map/4,
    reduce/4
]).

%% swirl_flow callbacks
map(_FlowId, _StreamName, Event, _MapperOpts) ->
    {update, {?L(type, Event), ?L(exchange_id, Event), ?L(bidder_id, Event)}, {1, 10}}.

reduce(_FlowId, _Period, Aggregates, ReducerOpts) ->
    case ?L(send_to , ReducerOpts) of
        undefined -> ok;
        Pid -> Pid ! Aggregates
    end.
