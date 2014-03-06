-module(swirl_mapper).
-include("swirl.hrl").
-compile({no_auto_import, [unregister/1]}).

%% public
-export([
    register/1,
    unregister/1
]).

%% internal
-export([
    lookup/1,
    map/6,
    start_link/4
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

-define(TABLE_NAME, aggregates).
-define(TABLE_OPTS, [public]).
-define(SERVER, ?MODULE).
-define(WIDTH, 16).

-record(state, {
    flow_id,
    flow_mod,
    flow_opts,
    table_id,
    reducer_node,
    flush_timer,
    flush_tstamp,
    heartbeat_timer,
    heartbeat_tstamp
}).

%% public
-spec lookup(binary()) -> list(tuple()).
lookup(FlowId) ->
    swirl_tracker:lookup(key(FlowId)).

-spec map(binary(), atom(), atom(), event(), term(), pos_integer()) -> ok.
map(FlowId, FlowMod, StreamName, Event, MapperOpts, TableId) ->
    case FlowMod:map(FlowId, StreamName, Event, MapperOpts) of
        Updates when is_list(Updates) ->
            lists:map(fun ({update, Key, Counters}) ->
                update(TableId, Key, Counters)
            end, Updates);
        {update, Key, Counters} ->
            update(TableId, Key, Counters);
        ignore ->
            ok
    end.

-spec register(binary()) -> true.
register(FlowId) ->
    swirl_tracker:register(key(FlowId), self()).

-spec unregister(binary()) -> true.
unregister(FlowId) ->
    swirl_tracker:unregister(key(FlowId)).

%% internal
start_link(FlowId, FlowMod, FlowOpts, ReducerNode) ->
    gen_server:start_link(?MODULE, {FlowId, FlowMod, FlowOpts, ReducerNode}, []).

%% gen_server callbacks
init({FlowId, FlowMod, FlowOpts, ReducerNode}) ->
    process_flag(trap_exit, true),
    register(FlowId),
    self() ! flush,
    self() ! heartbeat,

    {ok, #state {
        flow_id = FlowId,
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        reducer_node = ReducerNode,
        heartbeat_tstamp = swirl_utils:epoch_ms()
    }}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush, #state {
        flow_id = FlowId,
        table_id = TableId,
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        reducer_node = ReducerNode,
        flush_tstamp = Timestamp
    } = State) ->

    MapperFlush = ?L(mapper_flush, FlowOpts, ?DEFAULT_MAPPER_FLUSH),
    StreamName = ?L(stream_name, FlowOpts),

    {Timestamp2, FlushTimer} = swirl_utils:new_timer(MapperFlush, flush),
    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    swirl_flow:register(FlowId, FlowMod, FlowOpts, NewTableId),
    swirl_flow:unregister(FlowId, StreamName, TableId),

    Period = #period {start_at = Timestamp, end_at = Timestamp2},
    spawn(fun() -> flush_aggregates(FlowId, Period, TableId, ReducerNode) end),

    {noreply, State#state {
        table_id = NewTableId,
        flush_tstamp = Timestamp2,
        flush_timer = FlushTimer
    }};
handle_info(heartbeat, #state {
        flow_opts = FlowOpts,
        heartbeat_tstamp = Timestamp
    } = State) ->

    Heartbeat = ?L(heartbeat, FlowOpts, ?DEFAULT_HEARTBEAT),
    {Timestamp2, HeartbeatTimer} = swirl_utils:new_timer(Heartbeat, heartbeat),

    case Timestamp2 - Timestamp > 2 * Heartbeat of
        true ->
            {stop, normal, State#state {
                heartbeat_timer = HeartbeatTimer
            }};
        false ->
            {noreply, State#state {
                heartbeat_timer = HeartbeatTimer
            }}
    end;
handle_info({ping, ReducerNode}, #state {
        flow_id = FlowId,
        reducer_node = ReducerNode
    } = State) ->

    swirl_tracker:message(ReducerNode, FlowId, {pong, node()}),

    {noreply, State#state {
        heartbeat_tstamp = swirl_utils:epoch_ms()
    }};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow_id = FlowId,
        table_id = TableId,
        flow_opts = FlowOpts,
        flush_timer = FlushTimer,
        heartbeat_timer = HeartbeatTimer
    }) ->

    StreamName = ?L(stream_name, FlowOpts),
    swirl_flow:unregister(FlowId, StreamName, TableId),
    unregister(FlowId),
    timer:cancel(FlushTimer),
    timer:cancel(HeartbeatTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
flush_aggregates(_FlowId, _Period, undefined, _ReducerNode) ->
    ok;
flush_aggregates(FlowId, Period, TableId, ReducerNode) ->
    Aggregates = swirl_utils:tab2list(TableId),
    swirl_tracker:message(ReducerNode, FlowId, {mapper_flush, Period, Aggregates}),
    % to prevent unregister race condition
    timer:sleep(500),
    swirl_utils:safe_ets_delete(TableId).

key(FlowId) ->
    {mapper, FlowId}.

-spec update(pos_integer(), tuple(), tuple()) -> ok.
update(TableId, Key, Counters) ->
    Rnd = erlang:system_info(scheduler_id) band (?WIDTH-1),
    swirl_utils:safe_ets_increment(TableId, {Key, Rnd}, Counters).
