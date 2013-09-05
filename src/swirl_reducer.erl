-module(swirl_reducer).
-include("swirl.hrl").
-compile({no_auto_import,[unregister/1]}).

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

-define(TABLE_NAME, counters).
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
    swirl_tracker:lookup({reducer, FlowId}).

-spec register(binary()) -> true.
register(FlowId) ->
    swirl_tracker:register({reducer, FlowId}, self()).

-spec unregister(binary()) -> true.
unregister(FlowId) ->
    swirl_tracker:unregister({reducer, FlowId}).

%% internal
reduce(FlowMod, FlowOpts, Period, CountersList) ->
    FlowMod:reduce(Period, CountersList, ?L(reducer_opts, FlowOpts)).

start_link(FlowId, FlowMod, FlowOpts) ->
    gen_server:start_link(?MODULE, {FlowId, FlowMod, FlowOpts}, []).

%% gen_server callbacks
init({FlowId, FlowMod, FlowOpts}) ->
    process_flag(trap_exit, true),
    register(FlowId),
    swirl_ets_manager:table(?TABLE_NAME, ?TABLE_OPTS, self()),
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

handle_info({'ETS-TRANSFER', NewTableId, _Pid,  {?TABLE_NAME, _Options, _Self}}, #state {
        flow_mod = FlowMod,
        flow_opts = FlowOpts,
        table_id = TableId,
        last_flush = Timstamp
    } = State) ->

    ReducerFlush = ?L(reducer_flush, FlowOpts, ?DEFAULT_REDUCER_FLUSH),
    {Timstamp2, TimerRef} = swirl_utils:new_timer(ReducerFlush, flush),
    Period = #period {start_at = Timstamp, end_at = Timstamp2},
    flush_counters(FlowMod, FlowOpts, Period, TableId),

    {noreply, State#state {
        table_id = NewTableId,
        last_flush = Timstamp2,
        timer_ref = TimerRef
    }};
handle_info(flush, State) ->
    swirl_ets_manager:new_table(?TABLE_NAME, ?TABLE_OPTS, self()),
    {noreply, State};
handle_info({mapper_flush, Period, CountersList}, #state {
        table_id = TableId
    } = State) ->

    map_counters(Period, CountersList, TableId),
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
flush_counters(_FlowMod, _FlowOpts, _Period, undefined) ->
    ok;
flush_counters(FlowMod, FlowOpts, Period, TableId) ->
    CountersList = ets:tab2list(TableId),
    true = ets:delete(TableId),
    reduce(FlowMod, FlowOpts, Period, CountersList).

map_counters(_Period, [], _TableId) ->
    ok;
map_counters(Period, [H | T], TableId) ->
    [Key| Counters] = tuple_to_list(H),
    UpdateOp = swirl_utils:update_op(Counters),
    NumCounters = length(Counters),
    swirl_utils:increment(Key, UpdateOp, NumCounters, TableId),
    map_counters(Period, T, TableId).
