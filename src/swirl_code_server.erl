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
-spec get_module(node(), module(), md5()) -> ok.
get_module(Node, Module, ModuleVsn) ->
    io:format("get_module: ~p ~p ~p~n", [Node, Module, ModuleVsn]),
    ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec version(module()) -> {ok, md5()} | {error, term()}.
version(Module) ->
    try Module:module_info(attributes) of
        Attributes ->
            [ModuleVsn] = ?L(vsn, Attributes),
            {ok, ModuleVsn}
    catch
        A:B ->
            {error, {A, B}}
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

handle_info(Info, State) ->
    io:format("unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
