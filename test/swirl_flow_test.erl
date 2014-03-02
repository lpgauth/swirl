-module(swirl_flow_test).
-export([
    after_suite/0,
    before_suite/0,
    test_benchmark_emit/0,
    test_swirl_flow/0
]).

-include_lib("etest/include/etest.hrl").

-define(N, 10000).

after_suite() ->
    ok = application:stop(swirl).

before_suite() ->
    random:seed(erlang:now()),
    application:ensure_all_started(swirl).

test_benchmark_emit() ->
    FlowIds = lists:map(fun (_) ->
        swirl_flow:start(swirl_flow_example, [
            {stream_name, video}
        ], [node()], node())
    end, lists:seq(1, 1)),

    timer:sleep(timer:seconds(1)),

    Timestamp = os:timestamp(),
    emit_loop(?N),
    Delta = timer:now_diff(os:timestamp(), Timestamp),

    io:format("~p microseconds~n", [Delta / ?N]),

    lists:map(fun (FlowId) ->
        swirl_flow:stop(FlowId, [node()], node())
    end, FlowIds).

test_swirl_flow() ->
    FlowId = swirl_flow:start(swirl_flow_example, [
        {stream_name, delivery},
        {stream_filter, "exchange_id = 3"},
        {mapper_flush, timer:seconds(1)},
        {reducer_flush, timer:seconds(1)},
        {reducer_opts, [
          {send_to, self()}
        ]}
    ], [node()], node()),

    timer:sleep(timer:seconds(1)),

    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 1}, {bidder_id, 10}]),
    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 3}, {bidder_id, 1}]),
    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 3}, {bidder_id, 10}]),

    Aggregates = receive_loop(),
    Expected = [{{start,3,10},1,10}, {{start,3,1},1,10}],

    ?assert_equal(Expected, Aggregates),
    swirl_flow:stop(FlowId, [node()], node()).

receive_loop() ->
    receive
        [] ->
            receive_loop();
        Aggregates ->
            Aggregates
    end.

%% private
emit_loop(0) ->
    ok;
emit_loop(N) ->
    swirl_stream:emit(video, random_event()),
    emit_loop(N-1).

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