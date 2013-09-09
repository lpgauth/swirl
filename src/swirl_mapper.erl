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
    map/5,
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

-define(TABLE_NAME, counters).
-define(TABLE_OPTS, [public]).
-define(SERVER, ?MODULE).
-define(WIDTH, 16).

-record(state, {
    flow_id,
    flow_mod,
    flow_opts,
    table_id,
    reducer_node,
    last_flush,
    timer_ref
}).

%% public
-spec lookup(binary()) -> list(tuple()).
lookup(FlowId) ->
    swirl_tracker:lookup(key(FlowId)).

-spec map(atom(), atom(), event(), term(), pos_integer()) -> ok.
map(FlowMod, StreamName, Event, MapperOpts, TableId) ->
    case FlowMod:map(StreamName, Event, MapperOpts) of
        {update, Key, Counters} ->
            Rnd = erlang:system_info(scheduler_id) band (?WIDTH-1),
            UpdateOp = swirl_utils:update_op(Counters),
            swirl_utils:safe_ets_increment(TableId, {Key, Rnd}, UpdateOp);
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

    {ok, #state {
        flow_id = FlowId,
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        reducer_node = ReducerNode
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
        last_flush = Timestamp
    } = State) ->

    MapperFlush = ?L(mapper_flush, FlowOpts, ?DEFAULT_MAPPER_FLUSH),
    {Timestamp2, TimerRef} = swirl_utils:new_timer(MapperFlush, flush),

    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    swirl_flow:register(FlowId, FlowMod, FlowOpts, NewTableId),
    StreamName = ?L(stream_name, FlowOpts),
    swirl_flow:unregister(FlowId, StreamName, TableId),

    Period = #period {start_at = Timestamp, end_at = Timestamp2},
    spawn(fun() -> flush_counters(FlowId, Period, TableId, ReducerNode) end),

    {noreply, State#state {
        table_id = NewTableId,
        last_flush = Timestamp2,
        timer_ref = TimerRef
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
        timer_ref = TimerRef
    }) ->

    StreamName = ?L(stream_name, FlowOpts),
    swirl_flow:unregister(FlowId, StreamName, TableId),
    unregister(FlowId),
    timer:cancel(TimerRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
flush_counters(_FlowId, _Period, undefined, _ReducerNode) ->
    ok;
flush_counters(FlowId, Period, TableId, ReducerNode) ->
    CountersList = ets:tab2list(TableId),
    swirl_tracker:message(ReducerNode, FlowId, {mapper_flush, Period, CountersList}),
    % to prevent unregister race condition
    timer:sleep(500),
    true = ets:delete(TableId).

key(FlowId) ->
    {mapper, FlowId}.
