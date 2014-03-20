-module(swirl_reducer).
-include("swirl.hrl").

-compile({no_auto_import, [
    unregister/1
]}).

%% internal
-export([
    lookup/1,
    reduce/3,
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

-define(TABLE_NAME, reducer_aggregates).
-define(TABLE_OPTS, [public, {write_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {
    flow,
    table_id,
    flush_timer,
    flush_tstamp,
    heartbeat_timer,
    heartbeat_nodes
}).

%% internal
-spec lookup(binary() | flow()) -> undefined | pid().
lookup(FlowId) when is_binary(FlowId) ->
    lookup(#flow {id = FlowId});
lookup(#flow {} = Flow) ->
    swirl_tracker:lookup(?TABLE_NAME_REDUCERS, key(Flow)).

-spec reduce(flow(), period(), list(tuple())) -> ok.
reduce(#flow {
        id = FlowId,
        module = Module,
        module_vsn = ModuleVsn,
        start_node = StartNode,
        reducer_opts = ReducerOpts
    }, Period, Aggregates) ->

    try Module:reduce(FlowId, Period, Aggregates, ReducerOpts)
    catch
        error:undef ->
            swirl_code_server:get_module(StartNode, Module, ModuleVsn)
    end.

-spec register(flow()) -> true.
register(#flow {} = Flow) ->
    swirl_tracker:register(?TABLE_NAME_REDUCERS, key(Flow), self()).

-spec start(flow()) -> {ok, pid()} | {error, reducers_max}.
start(#flow {} = Flow) ->
    ReducersCount = swirl_config:reducers_count(),
    ReducersMax = swirl_config:reducers_max(),
    case lookup(Flow) of
        undefined when ReducersCount < ReducersMax ->
            start_link(Flow);
        _Else ->
            {error, reducers_max}
    end.

-spec unregister(flow()) -> true.
unregister(#flow {} = Flow) ->
    swirl_tracker:unregister(?TABLE_NAME_REDUCERS, key(Flow)).

%% gen_server callbacks
init(#flow {mapper_nodes = MapperNodes} = Flow) ->
    process_flag(trap_exit, true),
    register(Flow),
    swirl_flow:register(Flow),

    self() ! flush,
    self() ! heartbeat,

    {ok, #state {
        flow = Flow,
        heartbeat_nodes = MapperNodes
    }}.
handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush, #state {
        flow = #flow {
            reducer_flush = ReducerFlush
        } = Flow,
        table_id = TableId,
        flush_tstamp = Tstamp
    } = State) ->

    {Tstamp2, FlushTimer} = swirl_utils:new_timer(ReducerFlush, flush),
    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    Period = #period {start_at = Tstamp, end_at = Tstamp2},
    spawn(fun() -> flush_aggregates(Flow, Period, TableId) end),

    {noreply, State#state {
        table_id = NewTableId,
        flush_tstamp = Tstamp2,
        flush_timer = FlushTimer
    }};
handle_info(heartbeat, #state {
        flow = #flow {
            id = FlowId,
            heartbeat = Heartbeat,
            mapper_nodes = MapperNodes
        } = Flow,
        heartbeat_nodes = HeartbeatNodes
    } = State) ->

    {_Tstamp, HeartbeatTimer} = swirl_utils:new_timer(Heartbeat, heartbeat),

    DeadNodes = lists:filter(fun(Node) ->
        not lists:member(Node, HeartbeatNodes)
    end, MapperNodes),

    Msg = {start_mapper, Flow},
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
handle_info({ping, Node}, #state {flow = #flow {id = FlowId}} = State) ->
    swirl_tracker:message(Node, FlowId, pong),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow = Flow,
        flush_timer = FlushTimer
    }) ->

    swirl_flow:unregister(Flow),
    unregister(Flow),
    timer:cancel(FlushTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
key(#flow {id = FlowId}) -> FlowId.

flush_aggregates(_Flow, _Period, undefined) ->
    ok;
flush_aggregates(Flow, Period, TableId) ->
    Aggregates = swirl_utils:tab2list(TableId),
    true = ets:delete(TableId),
    reduce(Flow, Period, Aggregates).

map_aggregates(_Period, [], _TableId) ->
    ok;
map_aggregates(Period, [H | T], TableId) ->
    [{Key, _} | Counters] = tuple_to_list(H),
    swirl_utils:safe_ets_increment(TableId, Key, Counters),
    map_aggregates(Period, T, TableId).

start_link(Flow) ->
    gen_server:start_link(?MODULE, Flow, []).
