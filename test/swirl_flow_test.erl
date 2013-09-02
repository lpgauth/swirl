-module(swirl_flow_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

before_suite() ->
    application:start(swirl).

test_swirl_flow() ->
    swirl_flow:start(swirl_flow_example, [
        {stream_name, delivery},
        {stream_filter, "exchange_id = 3"},
        {send_to, self()}
    ], [node()], node()),

    timer:sleep(timer:seconds(1)),
    swirl_stream:emit(delivery, [{exchange_id, 3}, {bidder_id, 10}]),

    Counters = receive_loop(),
    ?assert_equal([{{delivery, 3, 10}, 1, 10}], Counters).

receive_loop() ->
    receive
        [] ->
            receive_loop();
        Counters ->
            Counters
    end.

after_suite() ->
    application:stop(swirl).
