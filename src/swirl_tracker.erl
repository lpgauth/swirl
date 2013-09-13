-module(swirl_tracker).
-include("swirl.hrl").
-compile({no_auto_import, [
    register/2,
    unregister/1
]}).

%% public
-export([
    lookup/1,
    message/3,
    register/2,
    start_mappers/5,
    start_reducer/4,
    stop_mappers/2,
    stop_reducer/2,
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
    case ets:lookup(?TABLE_NAME, Key) of
        [{_, Value} | _] -> Value;
        [] -> undefined
    end.

-spec message(node(), binary(), term()) -> ok.
message(Node, FlowId, Msg) ->
    {swirl_tracker, Node} ! {flow, FlowId, Msg},
    ok.

-spec register(tuple(), term()) -> true.
register(Key, Value) ->
    ets:insert(?TABLE_NAME, {Key, Value}).

-spec start_mappers(binary(), atom(), [flow_opts()], [node()], node()) -> ok.
start_mappers(FlowId, FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    Msg = {start_mapper, FlowMod, FlowOpts, ReducerNode},
    [swirl_tracker:message(Node, FlowId, Msg) || Node <- MapperNodes],
    ok.

-spec start_reducer(binary(), atom(), [flow_opts()], node()) -> ok.
start_reducer(FlowId, FlowMod, FlowOpts, ReducerNode) ->
    Msg = {start_reducer, FlowMod, FlowOpts},
    swirl_tracker:message(ReducerNode, FlowId, Msg).

-spec stop_mappers(binary(), [node()]) -> ok.
stop_mappers(FlowId, MapperNodes) ->
    Msg = {stop_mapper, FlowId},
    [swirl_tracker:message(Node, FlowId, Msg) || Node <- MapperNodes],
    ok.

-spec stop_reducer(binary(), node()) -> ok.
stop_reducer(FlowId, ReducerNode) ->
    Msg = {stop_reducer, FlowId},
    swirl_tracker:message(ReducerNode, FlowId, Msg),
    ok.

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
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({flow, FlowId, Msg}, State) ->
    handler_flow_msg(FlowId, Msg, State);
handle_info(Info, State) ->
    io:format("unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
handler_flow_msg(FlowId, {mapper_flush, _Period, _Aggregates} = Msg, State) ->
    message(swirl_reducer:lookup(FlowId), Msg),
    {noreply, State};
handler_flow_msg(FlowId, {start_mapper, FlowMod, FlowOpts, ReducerNode}, State) ->
    swirl_mapper:start_link(FlowId, FlowMod, FlowOpts, ReducerNode),
    {noreply, State};
handler_flow_msg(FlowId, {start_reducer, FlowMod, FlowOpts}, State) ->
    swirl_reducer:start_link(FlowId, FlowMod, FlowOpts),
    {noreply, State};
handler_flow_msg(FlowId, {stop_mapper, FlowId}, State) ->
    message(swirl_mapper:lookup(FlowId), stop),
    {noreply, State};
handler_flow_msg(FlowId, {stop_reducer, FlowId}, State) ->
    message(swirl_reducer:lookup(FlowId), stop),
    {noreply, State}.

message(undefined, _Msg) ->
    ok;
message(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg.
