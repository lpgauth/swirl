-module(swirl_mapper).
-include("swirl.hrl").

-compile({no_auto_import, [
    unregister/1
]}).

%% internal
-export([
    lookup/1,
    map/3,
    register/1,
    start/1,
    unregister/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TABLE_NAME, mapper_aggregates).
-define(TABLE_OPTS, [public, {write_concurrency, true}]).
-define(SERVER, ?MODULE).
-define(WIDTH, 16).

-record(state, {
    flow,
    table_id,
    flush_timer,
    flush_tstamp,
    heartbeat_timer,
    heartbeat_tstamp
}).

%% internal
-spec lookup(binary() | flow()) -> undefined | pid.
lookup(FlowId) when is_binary(FlowId) ->
    lookup(#flow {id = FlowId});
lookup(#flow {} = Flow) ->
    swirl_tracker:lookup(?TABLE_NAME_MAPPERS, key(Flow)).

-spec map(atom(), event(), stream()) -> ok.
map(StreamName, Event, #stream {
        flow_id = FlowId,
        flow_mod = FlowMod,
        mapper_opts = MapperOpts,
        table_id = TableId
    }) ->

    try FlowMod:map(FlowId, StreamName, Event, MapperOpts) of
        Updates when is_list(Updates) ->
            [update(TableId, Key, Counters) ||
                {update, Key, Counters} <- Updates];
        {update, Key, Counters} ->
            update(TableId, Key, Counters);
        ignore ->
            ok
    catch
        error:undef ->
            % TODO: fetch module
            io:format("fetch module: ~p~n", [FlowMod])
    end.

-spec register(flow()) -> true.
register(#flow {} = Flow) ->
    swirl_tracker:register(?TABLE_NAME_MAPPERS, key(Flow), self()).

-spec start(flow()) -> {ok, pid()} | {error, mappers_max}.
start(#flow {} = Flow) ->
    MappersCount = swirl_config:mappers_count(),
    MappersMax = swirl_config:mappers_max(),
    case lookup(Flow) of
        undefined when MappersCount < MappersMax ->
            start_link(Flow);
        _Else -> ok
    end.

-spec unregister(flow()) -> true.
unregister(#flow {} = Flow) ->
    swirl_tracker:unregister(?TABLE_NAME_MAPPERS, key(Flow)).

%% gen_server callbacks
init(#flow {} = Flow) ->
    process_flag(trap_exit, true),
    register(Flow),
    swirl_flow:register(Flow),

    self() ! flush,
    self() ! heartbeat,

    {ok, #state {
        flow = Flow,
        heartbeat_tstamp = swirl_utils:unix_timestamp_ms()
    }}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush, #state {
        flow = #flow {
            mapper_flush = MapperFlush
        } = Flow,
        table_id = TableId,
        flush_tstamp = Tstamp
    } = State) ->

    {Tstamp2, FlushTimer} = swirl_utils:new_timer(MapperFlush, flush),
    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    swirl_stream:register(Flow, NewTableId),
    swirl_stream:unregister(Flow, TableId),

    Period = #period {start_at = Tstamp, end_at = Tstamp2},
    spawn(fun() -> flush_aggregates(Flow, Period, TableId) end),

    {noreply, State#state {
        table_id = NewTableId,
        flush_tstamp = Tstamp2,
        flush_timer = FlushTimer
    }};
handle_info(heartbeat, #state {
        flow = #flow {heartbeat = Heartbeat},
        heartbeat_tstamp = Tstamp
    } = State) ->

    {Tstamp2, HeartbeatTimer} = swirl_utils:new_timer(Heartbeat, heartbeat),
    NewState = State#state {
        heartbeat_timer = HeartbeatTimer
    },
    case Tstamp2 - Tstamp > 2 * Heartbeat of
        true -> {stop, normal, NewState};
        false -> {noreply, NewState}
    end;
handle_info({ping, ReducerNode}, #state {
        flow = #flow {
            id = FlowId,
            reducer_node = ReducerNode
        }
    } = State) ->

    swirl_tracker:message(ReducerNode, FlowId, {pong, node()}),
    {noreply, State#state {
        heartbeat_tstamp = swirl_utils:unix_timestamp_ms()
    }};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow = Flow,
        table_id = TableId,
        flush_timer = FlushTimer,
        heartbeat_timer = HeartbeatTimer
    }) ->

    swirl_stream:unregister(Flow, TableId),
    swirl_flow:unregister(Flow),
    unregister(Flow),
    timer:cancel(FlushTimer),
    timer:cancel(HeartbeatTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
flush_aggregates(_Flow, _Period, undefined) ->
    ok;
flush_aggregates(#flow {
        id = FlowId,
        reducer_node = ReducerNode
    }, Period, TableId) ->

    Aggregates = swirl_utils:tab2list(TableId),
    swirl_tracker:message(ReducerNode, FlowId, {mapper_flush, Period, Aggregates}),

    % to prevent unregister race condition
    timer:sleep(500),
    swirl_utils:safe_ets_delete(TableId).

key(#flow {id = Id}) -> Id.

start_link(Flow) ->
    gen_server:start_link(?MODULE, Flow, []).

-spec update(ets:tab(), tuple(), tuple()) -> ok.
update(TableId, Key, Counters) ->
    Rnd = erlang:system_info(scheduler_id) band (?WIDTH - 1),
    swirl_utils:safe_ets_increment(TableId, {Key, Rnd}, Counters).
