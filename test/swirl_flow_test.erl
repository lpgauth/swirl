-module(swirl_flow_test).
-include_lib("etest/include/etest.hrl").

-export([
    after_suite/0,
    before_suite/0,
    test_benchmark_emit/0,
    test_swirl_flow/0
]).

-define(N, 10000).

%% public
after_suite() ->
    ok = application:stop(swirl).

before_suite() ->
    random:seed(erlang:now()),
    application:ensure_all_started(swirl).

test_benchmark_emit() ->
    Flows = [new_flow() || _ <- lists:seq(1, 100)],
    timer:sleep(timer:seconds(1)),

    Timestamp = os:timestamp(),
    emit_loop(?N),
    Delta = timer:now_diff(os:timestamp(), Timestamp),
    io:format("~p microseconds~n", [Delta / ?N]),

    [swirl_flow:stop(Flow) || Flow <- Flows].

test_swirl_flow() ->
    {ok, Flow} = swirl_flow:start(swirl_flow_example, [
        {heartbeat, timer:seconds(10)},
        {mapper_opts, []},
        {mapper_window, timer:seconds(1)},
        {output_opts, [{send_to, self()}]},
        {reducer_opts, []},
        {reducer_skip, true},
        {reducer_window, timer:seconds(1)},
        {stream_filter, "exchange_id = 3"},
        {stream_names, [delivery, requests]},
        {window_sync, true}
    ], [node()], node()),

    timer:sleep(timer:seconds(1)),

    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 1}, {bidder_id, 10}]),
    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 3}, {bidder_id, 1}]),
    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 3}, {bidder_id, 10}]),
    swirl_stream:emit(requests, [{type, start}, {exchange_id, 3}, {bidder_id, 50}]),

    Rows = receive_loop(),
    Expected = [
        {{start,requests,3,50},{1,10}},
        {{start,delivery,3,1},{1,10}},
        {{start,delivery,3,10},{1,10}}
    ],

    ?assert_equal(Expected, Rows),
    swirl_flow:stop(Flow).

%% private
emit_loop(0) ->
    ok;
emit_loop(N) ->
    swirl_stream:emit(video, random_event()),
    emit_loop(N-1).

new_flow() ->
    FlowOpts = [
        {stream_names, [video]},
        {reducer_skip, true}
    ],
    {ok, Flow} = swirl_flow:start(swirl_flow_example, FlowOpts, [node()], node()),
    Flow.

random_event() ->
    Type = random_type(),
    ExchangeId = random:uniform(1000000),
    BidderId = random:uniform(100000),
    [{type, Type}, {exchange_id, ExchangeId}, {bidder_id, BidderId}].

random_type() ->
    lists:nth(random:uniform(11), [
        complete,
        firstQuartile,
        fullscreen,
        midpoint,
        mute,
        pause,
        resume,
        rewind,
        start,
        thirdQuartile,
        unmute
    ]).

receive_loop() ->
    receive
        [] ->
            receive_loop();
        Aggregates ->
            Aggregates
    end.
