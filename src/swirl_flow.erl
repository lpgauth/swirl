-module(swirl_flow).
-include("swirl.hrl").

%% public
-export([
    lookup/1,
    register/4,
    start/3,
    start/4,
    unregister/2
]).

%% callback
-callback map(atom(), event(), term()) -> {update, tuple(), tuple()} | ignore.
-callback reduce(period(), term(), term()) -> ok.

%% public
-spec lookup(atom()) -> [tuple()].
lookup(StreamName) ->
    ets:select(registry, match_spec(StreamName)).

-spec register(binary(), atom(), [flow_opts()], pos_integer()) -> true.
register(FlowId, FlowMod, FlowOpts, TableId) ->
    verify_options(FlowOpts, []),
    StreamName = ?L(stream_name, FlowOpts),
    StreamFilter = ?L(stream_filter, FlowOpts),
    ExpTree = expression_tree(StreamFilter),
    MapperOpts = ?L(mapper_opts, FlowOpts),
    Value = {ExpTree, FlowMod, MapperOpts, TableId},
    swirl_tracker:register(key(FlowId, StreamName), Value).

-spec start(atom(), [flow_opts()], [node()]) -> ok.
start(FlowMod, FlowOpts, MapperNodes) ->
    FlowId = swirl_utils:uuid(),
    swirl_tracker:start_mappers(FlowId, FlowMod, FlowOpts, MapperNodes, node()),
    swirl_reducer:register(FlowId),
    ok.

-spec start(atom(), [flow_opts()], [node()], node()) -> ok.
start(FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    FlowId = swirl_utils:uuid(),
    swirl_tracker:start_mappers(FlowId, FlowMod, FlowOpts, MapperNodes, ReducerNode),
    swirl_tracker:start_reducer(FlowId, FlowMod, FlowOpts, ReducerNode),
    ok.

-spec unregister(binary(), atom()) -> true.
unregister(FlowId, StreamName) ->
    swirl_tracker:unregister(key(FlowId, StreamName)).

%% private
expression_tree(undefined) ->
    undefined;
expression_tree(StreamFilter) ->
    {ok, ExpTree} = swirl_ql:parse(StreamFilter),
    ExpTree.

key(FlowId, StreamName) ->
    {flow, FlowId, StreamName}.

match_spec(StreamName) ->
    [{{{flow, '_', '$1'}, {'$2','$3','$4','$5'}}, [{'orelse' ,{'=:=', '$1', StreamName},
        {'=:=', '$1', undefined}}], [{{'$2','$3','$4','$5'}}]}].

verify_options([{mapper_flush, MapperFlush} | Options], Errors)
    when is_integer(MapperFlush) ->
        verify_options(Options, Errors);
verify_options([{mapper_opts, _} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{reducer_flush, ReducerFlush} | Options], Errors)
    when is_integer(ReducerFlush) ->
        verify_options(Options, Errors);
verify_options([{reducer_opts, _} | Options], Errors) ->
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
