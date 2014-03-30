swirl
=====
#### Examples: ####

##### Starting a flow: #####
    ok = application:start(swirl),
    ..
    FlowMod = swirl_flow_example,
    FlowOpts = [
        {stream_name, delivery},
        {stream_filter, "exchange_id = 3 AND bidder_id IS NOT NULL"}
    ],
    MapperNodes = [node()],
    ReducerNode = node(),
    {ok, Flow} = swirl_flow:start(FlowMod, FlowOpts, MapperNodes, ReducerNode),
    ..
    swirl_stream:emit(delivery, [{exchange_id, 1}, {bidder_id, 10}]),
    ..
    ok = swirl_flow:stop(Flow).

##### Implementing a flow: #####
    -module(swirl_flow_example).

    -behavior(swirl_flow).
    -export([
        map/3,
        reduce/3,
        output/4
    ]).

    %% swirl_flow callbacks
    map(_StreamName, Event, _MapperOpts) ->
        {{l(type, Event), l(exchange_id, Event), l(bidder_id, Event)}, {1, 10}}.

    reduce(_Flow, Row, _ReducerOpts) ->
        Row.

    output(_Flow, _Period, Rows, OutputOpts) ->
        case l(send_to , OutputOpts) of
            undefined -> ok;
            Pid -> Pid ! Rows
        end.

    %% helpers
    l(Key, Event) ->
        swirl_utils:lookup(Key, Event).

#### Web Interface: ####

    http://localhost:9090/

configurable via:

    application:set_env(swirl_ui, ip, {74,125,226,55}))
    application:set_env(swirl_ui, port, 9999))

#### Resource Limitation: ####

configurable via:

    application:set_env(swirl, mappers_max, 140))
    application:set_env(swirl, reducers_max, 200))

#### Function Details: ####

##### swirl_flow:start/4 ######

    start(FlowMod :: atom(), FlowOpts :: [flow_opts()], MapperNodes :: [node()], ReducerNode :: node()) -> ok

##### swirl_flow:stop/4 ######

    stop(Flow :: flow()) -> ok.

##### swirl_stream:emit/1 ######

    emit(StreamName :: atom(), Event :: event()) -> ok

##### data types: #####

    event() :: [{atom(), value()}].
    flow_opts() :: {stream_name, atom()} |
                   {stream_filter, string()} |
                   {mapper_window, pos_integer()} |
                   {mapper_opts, term()} |
                   {reducer_window, pos_integer()} |
                   {reducer_opts, term()} |
                   {heartbeat, pos_integer()}

#### TODO: ####
- node discovery
- boolean expression indexing
