-module(swirl_tracker).
-include("swirl.hrl").

-compile({no_auto_import, [
    register/2,
    unregister/1
]}).

%% public
-export([
    lookup/2,
    message/3,
    register/3,
    start_mappers/5,
    start_reducer/5,
    stop_mappers/2,
    stop_reducer/2,
    unregister/2
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

-define(TABLE_OPTS, [public, named_table, {read_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {}).

%% public
-spec lookup(atom(), tuple()) -> term().
lookup(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{_, Value} | _] -> Value;
        [] -> undefined
    end.

-spec message(node(), binary(), term()) -> ok.
message(Node, FlowId, Msg) ->
    {swirl_tracker, Node} ! {flow, FlowId, Msg},
    ok.

-spec register(atom(), tuple(), term()) -> true.
register(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}).

-spec start_mappers(binary(), atom(), [flow_opts()], [node()], node()) -> ok.
start_mappers(FlowId, FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    Msg = {start_mapper, FlowMod, FlowOpts, ReducerNode},
    [swirl_tracker:message(Node, FlowId, Msg) || Node <- MapperNodes],
    ok.

-spec start_reducer(binary(), atom(), [flow_opts()], [node()], node()) -> ok.
start_reducer(FlowId, FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    Msg = {start_reducer, FlowMod, FlowOpts, MapperNodes},
    swirl_tracker:message(ReducerNode, FlowId, Msg).

-spec stop_mappers(binary(), [node()]) -> ok.
stop_mappers(FlowId, MapperNodes) ->
    [swirl_tracker:message(Node, FlowId, stop_mapper) || Node <- MapperNodes],
    ok.

-spec stop_reducer(binary(), node()) -> ok.
stop_reducer(FlowId, ReducerNode) ->
    swirl_tracker:message(ReducerNode, FlowId, stop_reducer),
    ok.

-spec unregister(atom(), tuple()) -> true.
unregister(Table, Key) ->
    ets:delete(Table, Key).

%% internal
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    swirl_ets_manager:table(?TABLE_NAME_MAPPERS, ?TABLE_OPTS, swirl_tracker),
    swirl_ets_manager:table(?TABLE_NAME_REDUCERS, ?TABLE_OPTS, swirl_tracker),
    swirl_ets_manager:table(?TABLE_NAME_FLOWS, ?TABLE_OPTS, swirl_tracker),
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _TableId, _Pid,  {_TableName, _Options, ?SERVER}}, State) ->
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
handler_flow_msg(FlowId, {ping, _Node} = Msg, State) ->
    message(swirl_mapper:lookup(FlowId), Msg),
    {noreply, State};
handler_flow_msg(FlowId, {pong, _Node} = Msg, State) ->
    message(swirl_reducer:lookup(FlowId), Msg),
    {noreply, State};
handler_flow_msg(FlowId, {start_mapper, FlowMod, FlowOpts, ReducerNode}, State) ->
    case swirl_mapper:lookup(FlowId) of
        undefined ->
            swirl_mapper:start_link(FlowId, FlowMod, FlowOpts, ReducerNode);
        _Else ->
            ok
    end,
    {noreply, State};
handler_flow_msg(FlowId, {start_reducer, FlowMod, FlowOpts, MapperNodes}, State) ->
    case swirl_reducer:lookup(FlowId) of
        undefined ->
            swirl_reducer:start_link(FlowId, FlowMod, FlowOpts, MapperNodes);
        _Else ->
            ok
    end,
    {noreply, State};
handler_flow_msg(FlowId, stop_mapper, State) ->
    message(swirl_mapper:lookup(FlowId), stop),
    {noreply, State};
handler_flow_msg(FlowId, stop_reducer, State) ->
    message(swirl_reducer:lookup(FlowId), stop),
    {noreply, State}.

message(undefined, _Msg) ->
    ok;
message(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg.
