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
-spec start() -> ok.
start() ->
    {ok, _Apps} = application:ensure_all_started(?MODULE),
    ok.

%% application callbacks
start(_StartType, _StartArgs) ->
    swirl_sup:start_link().

stop(_State) ->
    ok.
