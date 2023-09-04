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
    unregister/1
]).

%% public
-spec emit(stream_name(), event()) -> ok.
emit(StreamName, Event) ->
    evaluate(StreamName, Event, lookup(StreamName)).

%% internal
-spec lookup(stream_name()) -> [tuple()].
lookup(StreamName) ->
    LookupSpec = match_lookup_spec(StreamName),
    ets:select(?TABLE_NAME_STREAMS, LookupSpec).

-spec register(flow(), ets:tab()) -> true.
register(#flow {
        id = FlowId,
        module = FlowMod,
        module_vsn = FlowModVsn,
        start_node = StartNode,
        stream_filter = StreamFilter,
        stream_names = StreamNames,
        mapper_opts = MapperOpts
    } = Flow, TableId) ->

    ok =:= lists:foreach(fun (StreamName) ->
        Key = key(Flow, StreamName),
        Stream = #stream {
            flow_id = FlowId,
            flow_mod = FlowMod,
            flow_mod_vsn = FlowModVsn,
            start_node = StartNode,
            exp_tree = expession_tree(StreamFilter),
            mapper_opts = MapperOpts,
            table_id = TableId
        },
        KeyValue = {Key, Stream},
        ets:insert(?TABLE_NAME_STREAMS, KeyValue)
    end, StreamNames).

-spec unregister(flow()) -> true.
unregister(#flow {stream_names = StreamNames} = Flow) ->
    ok =:= lists:foreach(fun (StreamName) ->
        DeleteSpec = match_delete_spec(Flow, StreamName),
        ets:match_delete(?TABLE_NAME_STREAMS, DeleteSpec)
    end, StreamNames).

%% private
evaluate(_StreamName, _Event, []) ->
    ok;
evaluate(StreamName, Event, [#stream {
        exp_tree = undefined
    } = Stream | T]) ->

    swirl_mapper:map(StreamName, Event, Stream),
    evaluate(StreamName, Event, T);
evaluate(StreamName, Event, [#stream {
        exp_tree = ExpTree
    } = Stream | T]) ->

    case swirl_ql:evaluate(ExpTree, Event) of
        true ->
            swirl_mapper:map(StreamName, Event, Stream);
        false -> ok
    end,
    evaluate(StreamName, Event, T).

expession_tree(undefined) ->
    undefined;
expession_tree(StreamFilter) ->
    {ok, ExpTree} = swirl_ql:parse(StreamFilter),
    ExpTree.

key(#flow {id = FlowId}, StreamName) ->
    {FlowId, StreamName}.

match_lookup_spec(StreamName) ->
    [{{{'$1', '$2'}, '$3'}, [{'orelse', {'=:=', '$2', StreamName},
        {'=:=', '$2', undefined}}], ['$3']}].

match_delete_spec(#flow {id = FlowId}, StreamName) ->
    {{FlowId, StreamName}, '_'}.
