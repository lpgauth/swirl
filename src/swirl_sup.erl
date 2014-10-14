-module(swirl_sup).
-include("swirl.hrl").

%% internal
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% internal
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
    Workers = [
        ?CHILD(swirl_ets_manager, worker),
        ?CHILD(swirl_code_server, worker),
        ?CHILD(swirl_tracker, worker)
    ],
    {ok, {{one_for_one, 5, 10}, Workers}}.
