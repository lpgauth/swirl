-module(swirl_flow).
-include("swirl.hrl").

%% public
-export([
    start/4,
    stop/1
]).

%% inernal
-export([
    lookup/1,
    register/1,
    unregister/1
]).

%% callback
-callback map(binary(), atom(), event(), term()) -> list(update()) | update() | ignore.
-callback reduce(binary(), period(), term(), term()) -> ok.

%% public
-spec start(atom(), [flow_opts()], [node()], node()) -> {ok, flow()} |
    {error, {bad_flow_opts, list()}}.
start(FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    case flow(FlowMod, FlowOpts, MapperNodes, ReducerNode) of
        {ok, Flow} ->
            ok = swirl_tracker:start_reducer(Flow),
            ok = swirl_tracker:start_mappers(Flow),
            {ok, Flow};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(flow()) -> ok.
stop(#flow {} = Flow) ->
    ok = swirl_tracker:stop_mappers(Flow),
    ok = swirl_tracker:stop_reducer(Flow),
    ok.

%% internal
-spec lookup(binary() | flow()) -> undefined | flow().
lookup(FlowId) when is_binary(FlowId) ->
    lookup(#flow {id = FlowId});
lookup(#flow {} = Flow) ->
    swirl_tracker:lookup(?TABLE_NAME_FLOWS, key(Flow)).

-spec register(flow()) -> true.
register(#flow {} = Flow) ->
    swirl_tracker:register(?TABLE_NAME_FLOWS, key(Flow), Flow).

-spec unregister(flow()) -> true.
unregister(#flow {} = Flow) ->
    swirl_tracker:unregister(?TABLE_NAME_FLOWS, key(Flow)).

%% private
flow(FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    case swirl_code_server:version(FlowMod) of
        {ok, ModuleVsn} ->
            case verify_options(FlowOpts) of
                ok ->
                    {ok, #flow {
                        id            = swirl_utils:uuid(),
                        module        = FlowMod,
                        module_vsn    = ModuleVsn,
                        start_node    = node(),
                        heartbeat     = ?L(heartbeat, FlowOpts, ?DEFAULT_HEARTBEAT),
                        mapper_flush  = ?L(mapper_flush, FlowOpts, ?DEFAULT_MAPPER_FLUSH),
                        mapper_nodes  = MapperNodes,
                        mapper_opts   = ?L(mapper_opts, FlowOpts, []),
                        reducer_flush = ?L(reducer_flush, FlowOpts, ?DEFAULT_REDUCER_FLUSH),
                        reducer_node  = ReducerNode,
                        reducer_opts  = ?L(reducer_opts, FlowOpts, []),
                        stream_filter = ?L(stream_filter, FlowOpts),
                        stream_name   = ?L(stream_name, FlowOpts),
                        timestamp     = os:timestamp()
                    }};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

key(#flow {id = Id}) -> Id.

verify_options(FlowOpts) ->
    verify_options(FlowOpts, []).

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
    {error, {bad_flow_opts, Errors}}.
