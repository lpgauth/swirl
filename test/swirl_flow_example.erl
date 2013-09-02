-module(swirl_flow_example).
-include("../include/swirl.hrl").

-behavior(swirl_flow).
-export([
    map/3,
    reduce/4
]).

%% swirl_mapper callbacks
map(StreamName, Event, _Opts) ->
    {update, {StreamName, ?L(exchange_id, Event), ?L(bidder_id, Event)}, {1, 10}}.

%% swirl_reducer callbacks
reduce(_Tstamp, _Tstamp2, Counters, Opts) ->
    ?L(send_to , Opts) ! Counters.
