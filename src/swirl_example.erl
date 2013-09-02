%% EXAMPLE:
%% swirl_flow:start(swirl_example, [{stream_name, delivery}, {stream_filter, "exchange_id = 3"}], [node()], swirl_example, [], node()).
%% swirl_stream:emit(delivery, [{exchange_id, 3}, {bidder_id, 10}]).

-module(swirl_example).
-include("swirl.hrl").

-behavior(swirl_mapper).
-behavior(swirl_reducer).
-export([
    map/3,
    reduce/4
]).

%% swirl_mapper callbacks
map(StreamName, Event, _Opts) ->
    {update, {StreamName, ?L(exchange_id, Event), ?L(bidder_id, Event)}, {1, 10}}.

%% swirl_reducer callbacks
reduce(Tstamp, Tstamp2, Counters, _Opts) ->
    io:format("~p ~p ~p~n", [Tstamp, Tstamp2, Counters]),
    ok.
