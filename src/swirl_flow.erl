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
-callback map(stream_name(), event(), mapper_opts()) ->
    list(update()) | update() | ignore.

-callback reduce(flow(), row(), reducer_opts()) ->
    update() | ignore.

-callback output(flow(), period(), list(row()), output_opts()) ->
    ok.

%% public
-spec start(atom(), [flow_opts()], [node()], node()) ->
    {ok, flow()} | {error, flow_mod_undef | {bad_flow_opts, list()}}.

start(FlowMod, FlowOpts, MapperNodes, ReducerNode) ->
    case flow(FlowMod, FlowOpts, MapperNodes, ReducerNode) of
        {ok, Flow} ->
            ok = swirl_tracker:start_reducer(Flow),
            ok = swirl_tracker:start_mappers(Flow),
            {ok, Flow};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(flow()) ->
    ok.

stop(#flow {} = Flow) ->
    ok = swirl_tracker:stop_mappers(Flow),
    ok = swirl_tracker:stop_reducer(Flow),
    ok.

%% internal
-spec lookup(binary() | flow()) ->
    undefined | flow().

lookup(FlowId) when is_binary(FlowId) ->
    lookup(#flow {id = FlowId});
lookup(#flow {} = Flow) ->
    swirl_tracker:lookup(?TABLE_NAME_FLOWS, key(Flow)).

-spec register(flow()) ->
    true.

register(#flow {} = Flow) ->
    swirl_tracker:register(?TABLE_NAME_FLOWS, key(Flow), Flow).

-spec unregister(flow()) ->
    true.

unregister(#flow {} = Flow) ->
    swirl_tracker:unregister(?TABLE_NAME_FLOWS, key(Flow)).

%% private
flow(Module, Options, MapperNodes, ReducerNode) ->
    case swirl_code_server:version(Module) of
        {ok, ModuleVsn} ->
            case verify_options(Options) of
                ok ->
                    {ok, new_flow_rec(Module, ModuleVsn, Options,
                        MapperNodes, ReducerNode)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, undef} ->
            {error, flow_mod_undef}
    end.

key(#flow {id = Id}) -> Id.

new_flow_rec(Module, ModuleVsn, Options, MapperNodes,
    ReducerNode) ->

    #flow {
        id             = swirl_utils:uuid(),
        module         = Module,
        module_vsn     = ModuleVsn,
        start_node     = node(),
        heartbeat      = ?L(heartbeat, Options, ?DEFAULT_HEARTBEAT),
        window_sync    = ?L(window_sync, Options, ?DEFAULT_WINDOW_SYNC),
        mapper_window  = ?L(mapper_window, Options, ?DEFAULT_MAPPER_WINDOW),
        mapper_nodes   = MapperNodes,
        mapper_opts    = ?L(mapper_opts, Options, []),
        reducer_window = ?L(reducer_window, Options, ?DEFAULT_REDUCER_WINDOW),
        reducer_node   = ReducerNode,
        reducer_opts   = ?L(reducer_opts, Options, []),
        reducer_skip   = ?L(reducer_skip, Options, ?DEFAULT_REDUCER_SKIP),
        output_opts    = ?L(output_opts, Options, []),
        stream_filter  = ?L(stream_filter, Options),
        stream_names   = ?L(stream_names, Options, []),
        started_at     = os:timestamp()
    }.

verify_options(FlowOpts) ->
    verify_options(FlowOpts, []).

verify_options([{heartbeat, Heartbeat} | Options], Errors)
    when is_integer(Heartbeat) ->
        verify_options(Options, Errors);
verify_options([{mapper_opts, _} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{mapper_window, MapperWindow} | Options], Errors)
    when is_integer(MapperWindow) ->
        verify_options(Options, Errors);
verify_options([{output_opts, _} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{reducer_opts, _} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{reducer_skip, ReducerSkip} | Options], Errors)
    when is_boolean(ReducerSkip) ->
        verify_options(Options, Errors);
verify_options([{reducer_window, ReducerWindow} | Options], Errors)
    when is_integer(ReducerWindow) ->
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
verify_options([{stream_names, StreamNames} | Options], Errors)
    when is_list(StreamNames)->
        verify_options(Options, Errors);
verify_options([{window_sync, WindowSync} | Options], Errors)
    when is_boolean(WindowSync) ->
        verify_options(Options, Errors);
verify_options([Option | Options], Errors) ->
    verify_options(Options, [Option | Errors]);
verify_options([], []) ->
    ok;
verify_options([], Errors) ->
    {error, {bad_flow_opts, Errors}}.
