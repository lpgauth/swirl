-module(swirl_flow_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

before_suite() ->
    application:start(swirl).

test_swirl_flow() ->
    swirl_flow:start(swirl_flow_example, [
        {stream_name, delivery},
        {stream_filter, "exchange_id = 3"},
        {mapper_flush, timer:seconds(1)},
        {reducer_flush, timer:seconds(1)},
        {reducer_opts, [
          {send_to, self()}
        ]}
    ], [node()], node()),

    timer:sleep(timer:seconds(1)),

    swirl_stream:emit(delivery, [{exchange_id, 1}, {bidder_id, 10}]),
    swirl_stream:emit(delivery, [{exchange_id, 3}, {bidder_id, 1}]),
    swirl_stream:emit(delivery, [{exchange_id, 3}, {bidder_id, 10}]),

    Counters = receive_loop(),
    Expected = [
        {{delivery,3,1},1,10},
        {{delivery,3,10},1,10}
    ],

    ?assert_equal(Expected, Counters).

receive_loop() ->
    receive
        [] ->
            receive_loop();
        Counters ->
            Counters
    end.

after_suite() ->
    application:stop(swirl).
