-module(swirl_reducer).
-include("swirl.hrl").

-compile({no_auto_import, [
    unregister/1
]}).

%% internal
-export([
    lookup/1,
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

-define(TABLE_NAME, reducer_rows).
-define(TABLE_OPTS, [public, {write_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {
    flow,
    table_id,
    window_timer,
    window_tstamp,
    hbeat_timer,
    hbeat_nodes
}).

%% internal
-spec lookup(binary() | flow()) -> undefined | pid().
lookup(FlowId) when is_binary(FlowId) ->
    lookup(#flow {id = FlowId});
lookup(#flow {} = Flow) ->
    swirl_tracker:lookup(?TABLE_NAME_REDUCERS, key(Flow)).

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
        hbeat_nodes = MapperNodes
    }}.
handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush, #state {
        flow = #flow {
            reducer_window = Window,
            window_sync = Sync
        } = Flow,
        table_id = TableId,
        window_tstamp = Tstamp
    } = State) ->

    Tstamp2 = swirl_utils:unix_tstamp_ms(),
    WindowTimer = swirl_utils:new_timer(Window, flush, Sync),
    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    Period = #period {start_at = Tstamp, end_at = Tstamp2},
    spawn(fun() -> flush_window(Flow, Period, TableId) end),

    {noreply, State#state {
        table_id = NewTableId,
        window_tstamp = Tstamp2,
        window_timer = WindowTimer
    }};
handle_info(heartbeat, #state {
        flow = #flow {
            id = FlowId,
            heartbeat = Hbeat,
            mapper_nodes = MapperNodes
        } = Flow,
        hbeat_nodes = HbeatNodes
    } = State) ->

    HbeatTimer = swirl_utils:new_timer(Hbeat, heartbeat, true),

    DeadNodes = lists:filter(fun(Node) ->
        not lists:member(Node, HbeatNodes)
    end, MapperNodes),

    FlowProp = swirl_utils:record_to_proplist(Flow),
    Msg = {start_mapper, FlowProp},
    [swirl_tracker:message(Node, FlowId, Msg) || Node <- DeadNodes],

    Msg2 = {ping, node()},
    [swirl_tracker:message(Node, FlowId, Msg2) || Node <- MapperNodes],

    {noreply, State#state {
        hbeat_timer = HbeatTimer,
        hbeat_nodes = []
    }};
handle_info({pong, MapperNode}, #state {
        hbeat_nodes = HbeatNodes
    } = State) ->

    {noreply, State#state {
        hbeat_nodes = [MapperNode | HbeatNodes]
    }};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info({mapper_window, Period, Rows}, #state {
        table_id = TableId
    } = State) ->

    spawn(fun() -> map_rows(Period, Rows, TableId) end),
    {noreply, State};
handle_info({ping, Node}, #state {flow = #flow {id = FlowId}} = State) ->
    swirl_tracker:message(Node, FlowId, pong),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow = Flow,
        window_timer = WindowTimer
    }) ->

    swirl_flow:unregister(Flow),
    unregister(Flow),
    timer:cancel(WindowTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
flush_window(_Flow, _Period, undefined) ->
    ok;
flush_window(#flow {
        reducer_skip = ReducerSkip
    } = Flow, Period, TableId) ->

    Rows = swirl_utils:tab2list(TableId),
    true = ets:delete(TableId),
    ReducedRows = reduce_rows(Flow, Rows, ReducerSkip),
    output(Flow, Period, ReducedRows).

key(#flow {id = FlowId}) -> FlowId.

map_rows(_Period, [], _TableId) ->
    ok;
map_rows(Period, [H | T], TableId) ->
    [Key | Counters] = tuple_to_list(H),
    swirl_utils:safe_ets_increment(TableId, Key, Counters),
    map_rows(Period, T, TableId).

-spec output(flow(), period(), list(row())) -> ok.
output(#flow {
        module = Module,
        module_vsn = ModuleVsn,
        start_node = StartNode,
        output_opts = OutputOpts
    } = Flow, Period, Rows) ->

    try Module:output(Flow, Period, Rows, OutputOpts)
    catch
        error:undef ->
            swirl_code_server:get_module(StartNode, Module, ModuleVsn)
    end.

-spec reduce(flow(), row()) -> ignore | update().
reduce(#flow {
        module = Module,
        module_vsn = ModuleVsn,
        start_node = StartNode,
        reducer_opts = ReducerOpts
    } = Flow, Row) ->

    try Module:reduce(Flow, Row, ReducerOpts) of
        ignore -> ignore;
        {_Key, _Counters} = Update -> Update
    catch
        error:undef ->
            swirl_code_server:get_module(StartNode, Module, ModuleVsn),
            ignore
    end.

reduce_rows(_Flow, [], _ReducerSkip) ->
    [];
reduce_rows(Flow, [Row | T], ReducerSkip) ->
    [Key | Counters] = tuple_to_list(Row),
    Row2 = {Key, list_to_tuple(Counters)},
    case ReducerSkip of
        true ->
            [Row2 | reduce_rows(Flow, T, ReducerSkip)];
        false ->
            case reduce(Flow, Row2) of
                ignore ->
                    reduce_rows(Flow, T, ReducerSkip);
                Row3 ->
                    [Row3 | reduce_rows(Flow, T, ReducerSkip)]
            end
    end.

start_link(Flow) ->
    gen_server:start_link(?MODULE, Flow, []).
