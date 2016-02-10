-module(swirl_tracker).
-include("swirl.hrl").

-compile({no_auto_import, [
    register/2,
    unregister/1
]}).

%% internal
-export([
    lookup/2,
    message/3,
    register/3,
    start_link/0,
    start_mappers/1,
    start_reducer/1,
    stop_mappers/1,
    stop_reducer/1,
    unregister/2
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

%% internal
-spec lookup(ets:tab(), term()) -> term().
lookup(TableId, Key) ->
    swirl_utils:safe_ets_lookup_element(TableId, Key).

-spec message(node(), binary(), term()) -> ok.
message(Node, FlowId, Msg) ->
    {?SERVER, Node} ! {flow, FlowId, Msg},
    ok.

-spec register(ets:tab(), term(), term()) -> true.
register(TableId, Key, Value) ->
    ets:insert(TableId, {Key, Value}).

-spec start_link() -> {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_mappers(flow()) -> ok.
start_mappers(#flow {id = FlowId, mapper_nodes = MapperNodes} = Flow) ->
    FlowProp = swirl_utils:record_to_proplist(Flow),
    [message(Node, FlowId, {start_mapper, FlowProp}) || Node <- MapperNodes],
    ok.

-spec start_reducer(flow()) -> ok.
start_reducer(#flow {id = FlowId, reducer_node = ReducerNode} = Flow) ->
    FlowProp = swirl_utils:record_to_proplist(Flow),
    message(ReducerNode, FlowId, {start_reducer, FlowProp}).

-spec stop_mappers(flow()) -> ok.
stop_mappers(#flow {id = FlowId, mapper_nodes = MapperNodes}) ->
    [message(Node, FlowId, stop_mapper) || Node <- MapperNodes],
    ok.

-spec stop_reducer(flow()) -> ok.
stop_reducer(#flow {id = FlowId, reducer_node = ReducerNode}) ->
    message(ReducerNode, FlowId, stop_reducer),
    ok.

-spec unregister(ets:tab(), term()) -> true.
unregister(TableId, Key) ->
    ets:delete(TableId, Key).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    [swirl_ets_manager:table(Name, ?TABLE_OPTS, ?SERVER) || Name <- [
        ?TABLE_NAME_FLOWS,
        ?TABLE_NAME_MAPPERS,
        ?TABLE_NAME_REDUCERS,
        ?TABLE_NAME_STREAMS
    ]],
    {ok, #state {}}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _TableId, _Pid,  {_TableName, _Options, ?SERVER}},
    State) ->

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
handler_flow_msg(FlowId, {mapper_window, _Period, _Rows} = Msg, State) ->
    message(swirl_reducer:lookup(FlowId), Msg),
    {noreply, State};
handler_flow_msg(FlowId, {ping, _Node} = Msg, State) ->
    message(swirl_mapper:lookup(FlowId), Msg),
    {noreply, State};
handler_flow_msg(FlowId, {pong, _Node} = Msg, State) ->
    message(swirl_reducer:lookup(FlowId), Msg),
    {noreply, State};
handler_flow_msg(_FlowId, {start_mapper, FlowProp}, State) ->
    Flow = swirl_utils:proplist_to_record(FlowProp, flow),
    swirl_mapper:start(Flow),
    {noreply, State};
handler_flow_msg(_FlowId, {start_reducer, FlowProp}, State) ->
    Flow = swirl_utils:proplist_to_record(FlowProp, flow),
    swirl_reducer:start(Flow),
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
