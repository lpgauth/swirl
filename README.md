swirl
=====
#### Function Details: ####

##### swirl_flow:start/4 ######

    start(FlowMod :: atom(), FlowOpts :: [flow_opts()], MapperNodes :: [node()], ReducerNode :: node()) -> ok

##### swirl_stream:emit/1 ######

    emit(StreamName :: atom(), Event :: event()) -> ok

#### Data Types: ####

    event() :: [{atom(), value()}].
    flow_opts() :: {stream_name, atom()} | {stream_filter, string()}.

#### Example: ####

##### Implementing a flow: #####
    -module(swirl_flow_example).
    -include("swirl.hrl").

    -behavior(swirl_flow).
    -export([
        map/4,
        reduce/4
    ]).

    map(_FlowId, StreamName, Event, _MapperOpts) ->
        {update, {?L(exchange_id, Event), ?L(bidder_id, Event)}, {1, 10}}.

    reduce(_FlowId, _Period, Aggregates, _ReducerOpts) ->
        io:format("~p~n", [Aggregates]).

##### Starting flow: #####

    FlowMod = swirl_flow_example,
    FlowOpts = [
        {stream_name, delivery},
        {stream_filter, "exchange_id = 3 AND bidder_id IS NOT NULL"}
    ],
    MapperNodes = [node()],
    ReducerNode = node(),
    swirl_flow:start(FlowMod, FlowOpts, MapperNodes, ReducerNode),

##### Emitting to stream: #####

    swirl_stream:emit(delivery, [{exchange_id, 1}, {bidder_id, 10}])

#### TODO: ####
- heartbeat
- node discovery
- code distribution
