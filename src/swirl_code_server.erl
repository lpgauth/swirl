-module(swirl_code_server).
-include("swirl.hrl").

%% public
-export([
    get_module/3,
    start_link/0,
    version/1
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

-define(TABLE_OPTS, [public, named_table, {write_concurrency, true}]).
-define(SERVER, ?MODULE).

-record(state, {}).

%% public
-spec get_module(node(), module(), module_vsn()) -> ok.
get_module(Node, Module, ModuleVsn) ->
    KeyVal = {{Module, ModuleVsn}, true},
    case ets:insert_new(?TABLE_NAME_CODE_SERVER, KeyVal) of
        true ->
            message(Node, {get_module, Module, ModuleVsn, node()});
        false -> ok
    end.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec version(module()) -> {ok, module_vsn()} | {error, term()}.
version(Module) ->
    try Module:module_info(attributes) of
        Attributes ->
            [ModuleVsn] = ?L(vsn, Attributes),
            {ok, ModuleVsn}
    catch
        error:undef ->
            {error, undef}
    end.

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    swirl_ets_manager:table(?TABLE_NAME_CODE_SERVER, ?TABLE_OPTS, ?SERVER),
    {ok, #state {}}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _TableId, _Pid, _Data}, State) ->
    {noreply, State};
handle_info({get_module, Module, ModuleVsn, Node}, State) ->
    {ok, ModuleVsn} = version(Module),
    {Module, Binary, Filename} = code:get_object_code(Module),
    message(Node, {load_module, Module, Binary, Filename}),
    {noreply, State};
handle_info({load_module, Module, Binary, Filename}, State) ->
    {module, Module} = code:load_binary(Module, Filename, Binary),
    {noreply, State};
handle_info(Info, State) ->
    io:format("unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
message(Node, Msg) ->
    {?SERVER, Node} ! Msg,
    ok.
