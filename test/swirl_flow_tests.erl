-module(swirl_flow_tests).
-include("test.hrl").

-compile(export_all).

%% runners
swirl_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        ?T(test_flow)
    ]}.

%% tests
test_flow() ->
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

    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 1},
        {bidder_id, 10}]),
    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 3},
        {bidder_id, 1}]),
    swirl_stream:emit(delivery, [{type, start}, {exchange_id, 3},
        {bidder_id, 10}]),
    swirl_stream:emit(requests, [{type, start}, {exchange_id, 3},
        {bidder_id, 50}]),

    Rows = receive_loop(),
    Expected = [
        {{start, delivery, 3, 1}, {1, 10}},
        {{start, delivery, 3, 10}, {1, 10}},
        {{start, requests, 3, 50}, {1, 10}}
    ],

    ?assertEqual(Expected, lists:usort(Rows)),
    swirl_flow:stop(Flow).

%% utils
cleanup() ->
    error_logger:tty(false),
    application:stop(swirl),
    error_logger:tty(true).

receive_loop() ->
    receive
        [] -> receive_loop();
        Aggregates -> Aggregates
    end.

setup() ->
    error_logger:tty(false),
    application:stop(swirl),
    swirl:start(),
    error_logger:tty(true).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
