-module(swirl_ets_manager).
-include("swirl.hrl").

%% public
-export([
    new_table/3,
    table/3
]).

%% internal
-export([
    start_link/0
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

-define(SERVER, ?MODULE).

-record(state, {
    tables
}).

%% public
-spec new_table(atom(), list(atom() | tuple()), atom() | pid()) -> ok.
new_table(Name, Options, Server) ->
    gen_server:cast(?SERVER, {new_table, {Name, Options, Server}}).

-spec table(atom(), list(atom() | tuple()), atom() | pid()) -> ok.
table(Name, Options, Server) ->
    gen_server:cast(?SERVER, {table, {Name, Options, Server}}).

%% internal
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    {ok, #state {
        tables = dict:new()
    }}.

handle_call(Request, _From, State) ->
    io:format("unexpected message: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast({new_table, {Name, Options, Server} = Data}, #state {
        tables = Tables
    } = State) ->

    TableId = ets_new(Name, Options, Data),
    Tables2 = dict:erase({Server, Name}, Tables),
    Tables3 = dict:append({Server, Name}, TableId, Tables2),
    ets_give_away(Server, TableId, Data),

    {noreply, State#state {
        tables = Tables3
    }};
handle_cast({table, {Name, Options, Server} = Data}, #state {
        tables = Tables
    } = State) ->

    TableId =
        case swirl_utils:safe_dict_fetch({Server, Name}, Tables) of
            undefined -> ets_new(Name, Options, Data);
            [TableId2] -> TableId2
        end,
    Tables2 = dict:erase({Server, Name}, Tables),
    Tables3 = dict:append({Server, Name}, TableId, Tables2),
    ets_give_away(Server, TableId, Data),

    {noreply, State#state {
        tables = Tables3
    }};
handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _TableId, _Pid, _Data}, State) ->
    {noreply, State};
handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
ets_new(Name, Options, Data) ->
    TableId = ets:new(Name, Options),
    true = ets:setopts(TableId, {heir, self(), Data}),
    TableId.

ets_give_away(Server, TableId, Data) ->
    ServerPid = server_pid(Server),
    ets:give_away(TableId, ServerPid, Data).

server_pid(Server) when is_pid(Server) ->
    Server;
server_pid(Server) when is_atom(Server) ->
    whereis(Server).
