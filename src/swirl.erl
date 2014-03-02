-module(swirl).

%% public
-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
start() ->
    application:ensure_all_started(?MODULE).

%% application callbacks
start(_StartType, _StartArgs) ->
    swirl_sup:start_link().

stop(_State) ->
    ok.
