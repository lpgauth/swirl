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

-define(TABLE_NAME, mapper_rows).
-define(TABLE_OPTS, [public, {write_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {
    flow,
    table_id,
    window_timer,
    window_tstamp,
    hbeat_timer,
    hbeat_tstamp
}).

%% internal
-spec lookup(binary() | flow()) -> undefined | pid.
lookup(FlowId) when is_binary(FlowId) ->
    lookup(#flow {id = FlowId});
lookup(#flow {} = Flow) ->
    swirl_tracker:lookup(?TABLE_NAME_MAPPERS, key(Flow)).

-spec map(atom(), event(), stream()) -> ok.
map(StreamName, Event, #stream {
        flow_mod = FlowMod,
        flow_mod_vsn = FlowModVsn,
        start_node = StartNode,
        mapper_opts = MapperOpts,
        table_id = TableId
    }) ->

    try FlowMod:map(StreamName, Event, MapperOpts) of
        Updates when is_list(Updates) ->
            [update(TableId, Key, Counters) || {Key, Counters} <- Updates];
        {Key, Counters} ->
            update(TableId, Key, Counters);
        ignore ->
            ok
    catch
        error:undef ->
            swirl_code_server:get_module(StartNode, FlowMod, FlowModVsn)
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
        hbeat_tstamp = swirl_utils:unix_tstamp_ms()
    }}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush, #state {
        flow = #flow {
            mapper_window = Window,
            window_sync = Sync
        } = Flow,
        table_id = TableId,
        window_tstamp = Tstamp
    } = State) ->

    Tstamp2 = swirl_utils:unix_tstamp_ms(),
    WindowTimer = swirl_utils:new_timer(Window, flush, Sync),
    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    swirl_stream:register(Flow, NewTableId),

    Period = #period {start_at = Tstamp, end_at = Tstamp2},
    spawn(fun() -> flush_window(Flow, Period, TableId) end),

    {noreply, State#state {
        table_id = NewTableId,
        window_tstamp = Tstamp2,
        window_timer = WindowTimer
    }};
handle_info(heartbeat, #state {
        flow = #flow {
            heartbeat = Hbeat
        },
        hbeat_tstamp = Tstamp
    } = State) ->

    Tstamp2 = swirl_utils:unix_tstamp_ms(),
    HbeatTimer = swirl_utils:new_timer(Hbeat, heartbeat, true),
    Delta = Tstamp2 - Tstamp,

    case Delta > 2 * Hbeat of
        true ->
            {stop, normal, State};
        false ->
            {noreply, State#state {
                hbeat_timer = HbeatTimer
            }}
    end;
handle_info({ping, ReducerNode}, #state {
        flow = #flow {
            id = FlowId,
            reducer_node = ReducerNode
        }
    } = State) ->

    Msg = {pong, node()},
    swirl_tracker:message(ReducerNode, FlowId, Msg),

    {noreply, State#state {
        hbeat_tstamp = swirl_utils:unix_tstamp_ms()
    }};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow = Flow,
        window_timer = WindowTimer,
        hbeat_timer = HbeatTimer
    }) ->

    swirl_stream:unregister(Flow),
    swirl_flow:unregister(Flow),
    unregister(Flow),
    timer:cancel(WindowTimer),
    timer:cancel(HbeatTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
flush_window(_Flow, _Period, undefined) ->
    ok;
flush_window(#flow {
        id = FlowId,
        reducer_node = ReducerNode
    }, Period, TableId) ->

    Rows = swirl_utils:tab2list(TableId),
    Msg = {mapper_window, Period, Rows},
    swirl_tracker:message(ReducerNode, FlowId, Msg),
    swirl_utils:safe_ets_delete(TableId).

key(#flow {id = Id}) -> Id.

start_link(Flow) ->
    gen_server:start_link(?MODULE, Flow, []).

-spec update(ets:tab(), tuple(), tuple()) -> ok.
update(TableId, Key, Counters) ->
    swirl_utils:safe_ets_increment(TableId, Key, Counters).
