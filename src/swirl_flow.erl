-module(swirl_flow).
-include("swirl.hrl").

%% public
-export([
    lookup/1,
    register/4,
    start/3,
    start/6,
    unregister/2
]).

%% public
lookup(StreamName) ->
    ets:select(registry, match_spec(StreamName)).

register(FlowId, MapperMod, MapperOpts, TableId) ->
    StreamName = ?L(stream_name, MapperOpts),
    StreamFilter = ?L(stream_filter, MapperOpts),
    ExpTree = expression_tree(StreamFilter),
    Value = {ExpTree, MapperMod, MapperOpts, TableId},
    swirl_tracker:register(key(FlowId, StreamName), Value).

start(MapperMod, MapperOpts, MapperNodes) ->
    FlowId = swirl_utils:uuid(),
    swirl_tracker:start_mappers(FlowId, MapperMod, MapperOpts, MapperNodes, node()),
    swirl_reducer:register(FlowId).

start(MapperMod, MapperOpts, MapperNodes, ReducerMod, ReducerOpts, ReducerNode) ->
    FlowId = swirl_utils:uuid(),
    swirl_tracker:start_mappers(FlowId, MapperMod, MapperOpts, MapperNodes, ReducerNode),
    swirl_tracker:start_reducer(FlowId, ReducerMod, ReducerOpts, ReducerNode).

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
    [{{{flow, '_', StreamName}, {'$1','$2','$3','$4'}}, [], [{{'$1','$2','$3','$4'}}]}].
