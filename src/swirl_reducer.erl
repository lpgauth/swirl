%% TODO: make flush configurable

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
    reduce/5,
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
    reducer_module,
    reducer_options,
    reducer_tid,
    flush_tstamp,
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
reduce(ReduceMod, ReducerOpts, Tstamp, NewTstamp, CountersList) ->
    ReduceMod:reduce(Tstamp, NewTstamp, CountersList, ReducerOpts).

start_link(FlowId, ReducerMod, ReducerOpts) ->
    gen_server:start_link(?MODULE, {FlowId, ReducerMod, ReducerOpts}, []).

%% gen_server callbacks
init({FlowId, ReducerMod, ReducerOpts}) ->
    process_flag(trap_exit, true),
    register(FlowId),
    swirl_ets_manager:table(?TABLE_NAME, ?TABLE_OPTS, self()),
    {ok, #state {
        flow_id = FlowId,
        reducer_module = ReducerMod,
        reducer_options = ReducerOpts
    }}.
handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(flush_counters, State) ->
    swirl_ets_manager:new_table(?TABLE_NAME, ?TABLE_OPTS, self()),
    {noreply, State};
handle_info({flush_counters, _FlowId, _Tstamp, _NewTstamp, CountersList}, #state {
        reducer_tid = TableId
    } = State) ->

    map_counters(CountersList, TableId),
    {noreply, State};
handle_info({'ETS-TRANSFER', NewTableId, _Pid,  {?TABLE_NAME, _Options, _Self}}, #state {
        reducer_module = ReduceMod,
        reducer_options = ReducerOpts,
        reducer_tid = TableId,
        flush_tstamp = Tstamp
    } = State) ->

    {NewTstamp, TimerRef} = swirl_utils:new_timer(?DEFAULT_REDUCER_FLUSH, flush_counters),
    flush_counters(ReduceMod, ReducerOpts, Tstamp, NewTstamp, TableId),

    {noreply, State#state {
        reducer_tid = NewTableId,
        flush_tstamp = NewTstamp,
        timer_ref = TimerRef
    }};
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
flush_counters(_ReduceMod, _ReducerOpts, _Tstamp, _NewTstamp, undefined) ->
    ok;
flush_counters(ReduceMod, ReducerOpts, Tstamp, NewTstamp, TableId) ->
    CountersList = ets:tab2list(TableId),
    true = ets:delete(TableId),
    reduce(ReduceMod, ReducerOpts, Tstamp, NewTstamp, CountersList).

map_counters([], _TableId) ->
    ok;
map_counters([H | T], TableId) ->
    [Key| Counters] = tuple_to_list(H),
    UpdateOp = swirl_utils:update_op(Counters),
    NumCounters = length(Counters),
    swirl_utils:increment(Key, UpdateOp, TableId, NumCounters),
    map_counters(T, TableId).
