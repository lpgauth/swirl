-module(swirl_reducer).
-include("swirl.hrl").
-compile({no_auto_import, [unregister/1]}).

%% public
-export([
    lookup/1,
    register/1,
    unregister/1
]).

%% internal
-export([
    reduce/4,
    start_link/3
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
-define(TABLE_OPTS, [public, {write_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {
    flow_id,
    flow_mod,
    flow_opts,
    table_id,
    last_flush,
    timer_ref
}).

%% public
-spec lookup(binary()) -> list(tuple()).
lookup(FlowId) ->
    swirl_tracker:lookup(key(FlowId)).

-spec register(binary()) -> true.
register(FlowId) ->
    swirl_tracker:register(key(FlowId), self()).

-spec unregister(binary()) -> true.
unregister(FlowId) ->
    swirl_tracker:unregister(key(FlowId)).

%% internal
reduce(FlowMod, FlowOpts, Period, Aggregates) ->
    FlowMod:reduce(Period, Aggregates, ?L(reducer_opts, FlowOpts, [])).

start_link(FlowId, FlowMod, FlowOpts) ->
    gen_server:start_link(?MODULE, {FlowId, FlowMod, FlowOpts}, []).

%% gen_server callbacks
init({FlowId, FlowMod, FlowOpts}) ->
    process_flag(trap_exit, true),
    register(FlowId),
    self() ! flush,

    {ok, #state {
        flow_id = FlowId,
        flow_mod = FlowMod,
        flow_opts = FlowOpts
    }}.
handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush, #state {
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        table_id = TableId,
        last_flush = Timstamp
    } = State) ->

    ReducerFlush = ?L(reducer_flush, FlowOpts, ?DEFAULT_REDUCER_FLUSH),
    {Timstamp2, TimerRef} = swirl_utils:new_timer(ReducerFlush, flush),
    NewTableId = ets:new(?TABLE_NAME, ?TABLE_OPTS),
    Period = #period {start_at = Timstamp, end_at = Timstamp2},
    spawn(fun() -> flush_aggregates(FlowMod, FlowOpts, Period, TableId) end),

    {noreply, State#state {
        table_id = NewTableId,
        last_flush = Timstamp2,
        timer_ref = TimerRef
    }};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info({mapper_flush, Period, Aggregates}, #state {
        table_id = TableId
    } = State) ->

    spawn(fun() -> map_aggregates(Period, Aggregates, TableId) end),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow_id = FlowId,
        timer_ref = TimerRef
    }) ->

    unregister(FlowId),
    timer:cancel(TimerRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
key(FlowId) ->
    {reducer, FlowId}.

flush_aggregates(_FlowMod, _FlowOpts, _Period, undefined) ->
    ok;
flush_aggregates(FlowMod, FlowOpts, Period, TableId) ->
    Aggregates = ets:tab2list(TableId),
    true = ets:delete(TableId),
    reduce(FlowMod, FlowOpts, Period, Aggregates).

map_aggregates(_Period, [], _TableId) ->
    ok;
map_aggregates(Period, [H | T], TableId) ->
    [{Key, _} | Counters] = tuple_to_list(H),
    UpdateOp = swirl_utils:update_op(Counters),
    swirl_utils:safe_ets_increment(TableId, Key, UpdateOp),
    map_aggregates(Period, T, TableId).
