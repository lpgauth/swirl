-module(swirl_stream).
-include("swirl.hrl").
-compile([native]).

%% public
-export([
    emit/2
]).

%% internal
-export([
    lookup/1,
    register/2,
    unregister/2
]).

%% public
-spec emit(atom(), event()) -> ok.
emit(StreamName, Event) ->
    evaluate(StreamName, Event, lookup(StreamName)).

%% internal
-spec lookup(atom() | flow()) -> [tuple()].
lookup(StreamName) when is_atom(StreamName) ->
    lookup(#flow {stream_name = StreamName});
lookup(Flow) ->
    ets:select(?TABLE_NAME_STREAMS, match_lookup_spec(Flow)).

-spec register(flow(), erlang:tab()) -> true.
register(#flow {
        stream_filter = StreamFilter,
        module = FlowMod,
        mapper_opts = MapperOpts
    } = Flow, TableId) ->

    ExpTree = expression_tree(StreamFilter),
    Value = {ExpTree, FlowMod, MapperOpts, TableId},
    ets:insert(?TABLE_NAME_STREAMS, {key(Flow), Value}).

-spec unregister(flow(), erlang:tab()) -> true.
unregister(Flow, TableId) ->
    DeleteSpec = match_delete_spec(Flow, TableId),
    ets:select_delete(?TABLE_NAME_STREAMS, DeleteSpec),
    true.

%% private
evaluate(_StreamName, _Event, []) ->
    ok;
evaluate(StreamName, Event, [{undefined, FlowId, FlowMod, MapperOpts, TableId} | T]) ->
    swirl_mapper:map(FlowId, FlowMod, StreamName, Event, MapperOpts, TableId),
    evaluate(StreamName, Event, T);
evaluate(StreamName, Event, [{ExpTree, FlowId, FlowMod, MapperOpts, TableId} | T]) ->
    case swirl_ql:evaluate(ExpTree, Event) of
        true ->
            swirl_mapper:map(FlowId, FlowMod, StreamName, Event, MapperOpts, TableId);
        false -> ok
    end,
    evaluate(StreamName, Event, T).

expression_tree(undefined) ->
    undefined;
expression_tree(StreamFilter) ->
    {ok, ExpTree} = swirl_ql:parse(StreamFilter),
    ExpTree.

key(#flow {id = FlowId, stream_name = StreamName}) ->
    {stream, FlowId, StreamName}.

match_lookup_spec(#flow {stream_name = StreamName}) ->
    [{{{stream, '$1', '$2'}, {'$3', '$4', '$5', '$6'}}, [{'orelse' , {'=:=', '$2', StreamName},
        {'=:=', '$2', undefined}}], [{{'$3', '$1', '$4', '$5', '$6'}}]}].

match_delete_spec(#flow {id = FlowId, stream_name = StreamName}, TableId) ->
    [{{{stream, FlowId, StreamName}, {'_', '_', '_', TableId}}, [], [true]}].
