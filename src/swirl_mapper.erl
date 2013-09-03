%% TODO: make flush configurable

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
-define(TABLE_OPTS, [public, {write_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {
    flow_id,
    mapper_module,
    mapper_options,
    mapper_tid,
    reducer_node,
    flush_tstamp,
    timer_ref
}).

%% public
-spec map(atom(), [flow_opts()], atom(), event(), pos_integer()) -> ok.
map(MapperMod, MapperOpts, StreamName, Event, TableId) ->
    case MapperMod:map(StreamName, Event, MapperOpts) of
        {update, Key, Counters} ->
            UpdateOp = swirl_utils:update_op(Counters),
            NumCounters = tuple_size(Counters),
            swirl_utils:increment(Key, UpdateOp, TableId, NumCounters);
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
start_link(FlowId, MapperMod, MapperOpts, ReducerNode) ->
    gen_server:start_link(?MODULE, {FlowId, MapperMod, MapperOpts, ReducerNode}, []).

%% gen_server callbacks
init({FlowId, MapperMod, MapperOpts, ReducerNode}) ->
    process_flag(trap_exit, true),
    register(FlowId),
    swirl_ets_manager:table(?TABLE_NAME, ?TABLE_OPTS, self()),
    {ok, #state {
        flow_id = FlowId,
        mapper_module = MapperMod,
        mapper_options = MapperOpts,
        reducer_node = ReducerNode
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
handle_info({'ETS-TRANSFER', NewTableId, _Pid,  {?TABLE_NAME, _Options, _Self}}, #state {
        flow_id = FlowId,
        mapper_tid = TableId,
        mapper_module = MapperMod,
        mapper_options = MapperOpts,
        reducer_node = ReducerNode,
        flush_tstamp = Tstamp
    } = State) ->

    swirl_flow:register(FlowId, MapperMod, MapperOpts, NewTableId),
    {NewTstamp, TimerRef} = swirl_utils:new_timer(?DEFAULT_MAPPER_FLUSH, flush_counters),
    flush_counters(FlowId, Tstamp, NewTstamp, TableId, ReducerNode),

    {noreply, State#state {
        mapper_tid = NewTableId,
        flush_tstamp = NewTstamp,
        timer_ref = TimerRef
    }};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state {
        flow_id = FlowId,
        mapper_options = MapperOpts,
        timer_ref = TimerRef
    }) ->

    StreamName = ?L(stream_name, MapperOpts),
    swirl_flow:unregister(FlowId, StreamName),
    unregister(FlowId),
    timer:cancel(TimerRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
flush_counters(_FlowId, _Tstamp, _NewTstamp, undefined, _ReducerNode) ->
    ok;
flush_counters(FlowId, Tstamp, NewTstamp, TableId, ReducerNode) ->
    CountersList = ets:tab2list(TableId),
    true = ets:delete(TableId),
    swirl_tracker:message(ReducerNode, {flush_counters, FlowId, Tstamp, NewTstamp, CountersList}).

key(FlowId) ->
    {mapper, FlowId}.
