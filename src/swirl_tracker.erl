-module(swirl_tracker).
-include("swirl.hrl").
-compile({no_auto_import, [
    register/2,
    unregister/1
]}).

%% public
-export([
    lookup/1,
    message/2,
    register/2,
    start_mappers/5,
    start_reducer/4,
    unregister/1
]).

%% internal
-export([
    start_link/0
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

-define(TABLE_NAME, registry).
-define(TABLE_OPTS, [public, named_table, {read_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {}).

%% public
-spec lookup(tuple()) -> term().
lookup(Key) ->
    [{_, Value} | _] = ets:lookup(?TABLE_NAME, Key),
    Value.

-spec message(node(), term()) -> ok.
message(Node, Msg) ->
    {swirl_tracker, Node} ! Msg.

-spec register(tuple(), term()) -> true.
register(Key, Value) ->
    ets:insert(?TABLE_NAME, {Key, Value}).

-spec start_mappers(binary(), atom(), [flow_opts()], [node()], node()) -> [ok].
start_mappers(FlowId, MapperMod, MapperOpts, MapperNodes, ReducerNode) ->
    Msg = {start_mapper, FlowId, MapperMod, MapperOpts, ReducerNode},
    [swirl_tracker:message(Node, Msg) || Node <- MapperNodes].

-spec start_reducer(binary(), atom(), [flow_opts()], node()) -> ok.
start_reducer(FlowId, ReducerMod, ReducerOpts, ReducerNode) ->
    Msg = {start_reducer, FlowId, ReducerMod, ReducerOpts},
    swirl_tracker:message(ReducerNode, Msg).

-spec unregister(tuple()) -> true.
unregister(Key) ->
    ets:delete(?TABLE_NAME, Key).

%% internal
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    swirl_ets_manager:table(?TABLE_NAME, ?TABLE_OPTS, swirl_tracker),
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _TableId, _Pid,  {?TABLE_NAME, _Options, ?SERVER}}, State) ->
    {noreply, State};
handle_info({flush_counters, FlowId, _Tstamp, _NewTstamp, _CountersList} = Msg, State) ->
    ReducerPid = swirl_reducer:lookup(FlowId),
    ReducerPid ! Msg,
    {noreply, State};
handle_info({start_mapper, FlowId, MapperMod, MapperOpts, ReducerNode}, State) ->
    swirl_mapper:start_link(FlowId, MapperMod, MapperOpts, ReducerNode),
    {noreply, State};
handle_info({start_reducer, FlowId, ReducerMod, ReducerOpts}, State) ->
    swirl_reducer:start_link(FlowId, ReducerMod, ReducerOpts),
    {noreply, State};
handle_info(Info, State) ->
    io:format("unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
