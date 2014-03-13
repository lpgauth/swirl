-module(swirl_flow).
-include("swirl.hrl").

%% public
-export([
    lookup/1,
    register/4,
    start/4,
    stop/3,
    unregister/3
]).

%% callback
-callback map(binary(), atom(), event(), term()) -> list(update()) | update() | ignore.
-callback reduce(binary(), period(), term(), term()) -> ok.

%% public
-spec lookup(atom()) -> [tuple()].
lookup(StreamName) ->
    ets:select(?TABLE_NAME_FLOWS, match_lookup_spec(StreamName)).

-spec register(binary(), atom(), [flow_opts()], pos_integer()) -> true.
register(FlowId, FlowMod, FlowOpts, TableId) ->
    verify_options(FlowOpts, []),
    StreamName = ?L(stream_name, FlowOpts),
    StreamFilter = ?L(stream_filter, FlowOpts),
    ExpTree = expression_tree(StreamFilter),
    MapperOpts = ?L(mapper_opts, FlowOpts, []),
    Value = {ExpTree, FlowMod, MapperOpts, TableId},
    swirl_tracker:register(?TABLE_NAME_FLOWS, key(FlowId, StreamName), Value).

-spec start(atom(), [flow_opts()], [node()], node()) -> binary().
start(FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    FlowId = swirl_utils:uuid(),
    swirl_tracker:start_reducer(FlowId, FlowMod, FlowOpts, MapperNodes, ReducerNode),
    swirl_tracker:start_mappers(FlowId, FlowMod, FlowOpts, MapperNodes, ReducerNode),
    FlowId.

-spec stop(binary(), [node()], node()) -> ok.
stop(FlowId, MapperNodes, ReducerNode) ->
    swirl_tracker:stop_mappers(FlowId, MapperNodes),
    swirl_tracker:stop_reducer(FlowId, ReducerNode),
    ok.

-spec unregister(binary(), atom(), pos_integer()) -> true.
unregister(FlowId, StreamName, TableId) ->
    ets:select_delete(?TABLE_NAME_FLOWS, match_delete_spec(FlowId, StreamName, TableId)).

%% private
expression_tree(undefined) ->
    undefined;
expression_tree(StreamFilter) ->
    {ok, ExpTree} = swirl_ql:parse(StreamFilter),
    ExpTree.

key(FlowId, StreamName) ->
    {flow, FlowId, StreamName}.

match_lookup_spec(StreamName) ->
    [{{{flow, '$1', '$2'}, {'$3', '$4', '$5', '$6'}}, [{'orelse' , {'=:=', '$2', StreamName},
        {'=:=', '$2', undefined}}], [{{'$3', '$1', '$4', '$5', '$6'}}]}].

match_delete_spec(FlowId, StreamName, TableId) ->
    [{{{flow, FlowId, StreamName}, {'_', '_', '_', TableId}}, [], [true]}].

verify_options([{mapper_flush, MapperFlush} | Options], Errors)
    when is_integer(MapperFlush) ->
        verify_options(Options, Errors);
verify_options([{mapper_heartbeat, MapperHeartbeat} | Options], Errors)
    when is_integer(MapperHeartbeat) ->
        verify_options(Options, Errors);
verify_options([{mapper_opts, _} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{reducer_flush, ReducerFlush} | Options], Errors)
    when is_integer(ReducerFlush) ->
        verify_options(Options, Errors);
verify_options([{reducer_opts, _} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{stream_filter, undefined} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{stream_filter, StreamFilter} = Option | Options], Errors) ->
    case swirl_ql:parse(StreamFilter) of
        {ok, _ExpTree} ->
            verify_options(Options, Errors);
        {error, _Reason} ->
            verify_options(Options, [Option | Errors])
    end;
verify_options([{stream_name, StreamName} | Options], Errors)
    when is_atom(StreamName)->
        verify_options(Options, Errors);
verify_options([Option | Options], Errors) ->
    verify_options(Options, [Option | Errors]);
verify_options([], []) ->
    ok;
verify_options([], Errors) ->
    erlang:error({bad_options, Errors}).
