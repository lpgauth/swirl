-module(swirl_reducer).
-include("swirl.hrl").

-compile({no_auto_import, [
    unregister/1
]}).

%% public
-export([
    lookup/1,
    register/1,
    unregister/1
]).

%% internal
-export([
    reduce/5,
    start/4
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

-define(TABLE_NAME, reducer_aggregates).
-define(TABLE_OPTS, [public, {write_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {
    flow_id,
    flow_mod,
    flow_opts,
    mapper_nodes,
    table_id,
    flush_timer,
    flush_tstamp,
    heartbeat_timer,
    heartbeat_nodes
}).

%% public
-spec lookup(binary()) -> list(tuple()).
lookup(FlowId) ->
    swirl_tracker:lookup(?TABLE_NAME_REDUCERS, key(FlowId)).

-spec register(binary()) -> true.
register(FlowId) ->
    swirl_tracker:register(?TABLE_NAME_REDUCERS, key(FlowId), self()).

-spec unregister(binary()) -> true.
unregister(FlowId) ->
    swirl_tracker:unregister(?TABLE_NAME_REDUCERS, key(FlowId)).

%% internal
reduce(FlowId, FlowMod, FlowOpts, Period, Aggregates) ->
    FlowMod:reduce(FlowId, Period, Aggregates, ?L(reducer_opts, FlowOpts, [])).

start(FlowId, FlowMod, FlowOpts, ReducerNode) ->
    case lookup(FlowId) of
        undefined ->
            start_link(FlowId, FlowMod, FlowOpts, ReducerNode);
        _Else -> ok
    end.

%% gen_server callbacks
init({FlowId, FlowMod, FlowOpts, MapperNodes}) ->
    process_flag(trap_exit, true),
    register(FlowId),
    self() ! flush,
    self() ! heartbeat,

    {ok, #state {
        flow_id = FlowId,
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        mapper_nodes = MapperNodes,
        heartbeat_nodes = MapperNodes
    }}.
handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush, #state {
        flow_id = FlowId,
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        table_id = TableId,
        flush_tstamp = Timestamp
    } = State) ->

    ReducerFlush = ?L(reducer_flush, FlowOpts, ?DEFAULT_REDUCER_FLUSH),
    {Timestamp2, FlushTimer} = swirl_utils:new_timer(ReducerFlush, flush),
    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    Period = #period {start_at = Timestamp, end_at = Timestamp2},
    spawn(fun() -> flush_aggregates(FlowId, FlowMod, FlowOpts, Period, TableId) end),

    {noreply, State#state {
        table_id = NewTableId,
        flush_tstamp = Timestamp2,
        flush_timer = FlushTimer
    }};
handle_info(heartbeat, #state {
        flow_id = FlowId,
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        mapper_nodes = MapperNodes,
        heartbeat_nodes = HeartbeatNodes
    } = State) ->

    Heartbeat = ?L(heartbeat, FlowOpts, ?DEFAULT_HEARTBEAT),
    {_Timestamp, HeartbeatTimer} = swirl_utils:new_timer(Heartbeat, heartbeat),

    DeadNodes = lists:filter(fun(Node) ->
        not lists:member(Node, HeartbeatNodes)
    end, MapperNodes),

    Msg = {start_mapper, FlowMod, FlowOpts, node()},
    [swirl_tracker:message(Node, FlowId, Msg) || Node <- DeadNodes],
    [swirl_tracker:message(Node, FlowId, Msg) || Node <- DeadNodes],

    Msg2 = {ping, node()},
    [swirl_tracker:message(Node, FlowId, Msg2) || Node <- MapperNodes],

    {noreply, State#state {
        heartbeat_timer = HeartbeatTimer,
        heartbeat_nodes = []
    }};
handle_info({pong, MapperNode}, #state {
        heartbeat_nodes = HeartbeatNodes
    } = State) ->

    {noreply, State#state {
        heartbeat_nodes = [MapperNode | HeartbeatNodes]
    }};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info({mapper_flush, Period, Aggregates}, #state {
        table_id = TableId
    } = State) ->

    spawn(fun() -> map_aggregates(Period, Aggregates, TableId) end),
    {noreply, State};
handle_info({ping, Node}, #state {flow_id = FlowId} = State) ->
    swirl_tracker:message(Node, FlowId, pong),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow_id = FlowId,
        flush_timer = FlushTimer
    }) ->

    unregister(FlowId),
    timer:cancel(FlushTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
key(FlowId) ->
    {reducer, FlowId}.

flush_aggregates(_FlowId, _FlowMod, _FlowOpts, _Period, undefined) ->
    ok;
flush_aggregates(FlowId, FlowMod, FlowOpts, Period, TableId) ->
    Aggregates = swirl_utils:tab2list(TableId),
    true = ets:delete(TableId),
    reduce(FlowId, FlowMod, FlowOpts, Period, Aggregates).

map_aggregates(_Period, [], _TableId) ->
    ok;
map_aggregates(Period, [H | T], TableId) ->
    [{Key, _} | Counters] = tuple_to_list(H),
    swirl_utils:safe_ets_increment(TableId, Key, Counters),
    map_aggregates(Period, T, TableId).

start_link(FlowId, FlowMod, FlowOpts, MapperNodes) ->
    gen_server:start_link(?MODULE, {FlowId, FlowMod, FlowOpts, MapperNodes}, []).
