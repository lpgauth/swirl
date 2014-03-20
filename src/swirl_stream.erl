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
lookup(#flow {} = Flow) ->
    ets:select(?TABLE_NAME_STREAMS, match_lookup_spec(Flow)).

-spec register(flow(), ets:tab()) -> true.
register(#flow {
        id = FlowId,
        module = FlowMod,
        module_vsn = FlowModVsn,
        start_node = StartNode,
        stream_filter = StreamFilter,
        mapper_opts = MapperOpts
    } = Flow, TableId) ->

    ets:insert(?TABLE_NAME_STREAMS, {key(Flow), #stream {
        flow_id = FlowId,
        flow_mod = FlowMod,
        flow_mod_vsn = FlowModVsn,
        start_node = StartNode,
        exp_tree = expession_tree(StreamFilter),
        mapper_opts = MapperOpts,
        table_id = TableId
    }}).

-spec unregister(flow(), ets:tab()) -> true.
unregister(#flow {} = Flow, TableId) ->
    DeleteSpec = match_delete_spec(Flow, TableId),
    ets:select_delete(?TABLE_NAME_STREAMS, DeleteSpec),
    true.

%% private
evaluate(_StreamName, _Event, []) ->
    ok;
evaluate(StreamName, Event, [{#stream {
        exp_tree = undefined
    } = Stream} | T]) ->

    swirl_mapper:map(StreamName, Event, Stream),
    evaluate(StreamName, Event, T);
evaluate(StreamName, Event, [{#stream {
        exp_tree = ExpTree
    } = Stream} | T]) ->

    case swirl_ql:evaluate(ExpTree, Event) of
        true -> swirl_mapper:map(StreamName, Event, Stream);
        false -> ok
    end,
    evaluate(StreamName, Event, T).

expession_tree(undefined) ->
    undefined;
expession_tree(StreamFilter) ->
    {ok, ExpTree} = swirl_ql:parse(StreamFilter),
    ExpTree.

key(#flow {id = FlowId, stream_name = StreamName}) ->
    {FlowId, StreamName}.

match_lookup_spec(#flow {stream_name = StreamName}) ->
    [{{{'$1', '$2'}, '$3'}, [{'orelse', {'=:=', '$2', StreamName},
        {'=:=', '$2', undefined}}], [{{'$3'}}]}].

match_delete_spec(#flow {id = FlowId, stream_name = StreamName}, TableId) ->
    [{{{FlowId, StreamName}, #stream {table_id = TableId}}, [], [true]}].
