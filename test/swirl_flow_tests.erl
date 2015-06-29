-module(swirl_flow_tests).
-include("test.hrl").

-compile(export_all).

%% runners
swirl_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    {inparallel, [
        ?T(test_benchmark_emit),
        ?T(test_swirl_flow)
    ]}}.

%% tests
test_benchmark_emit() ->
    Flows = [new_flow() || _ <- lists:seq(1, 100)],
    timer:sleep(timer:seconds(1)),

    Timestamp = os:timestamp(),
    emit_loop(?N),
    Delta = timer:now_diff(os:timestamp(), Timestamp),
    ?debugFmt("~p microseconds~n", [Delta / ?N]),

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

    swirl_stream:emit(delivery, #{type => start, exchange_id => 1, bidder_id => 10}),
    swirl_stream:emit(delivery, #{type => start, exchange_id => 3, bidder_id => 1}),
    swirl_stream:emit(delivery, #{type => start, exchange_id => 3, bidder_id => 10}),
    swirl_stream:emit(requests, #{type => start, exchange_id => 3, bidder_id => 50}),

    Rows = receive_loop(),
    Expected = [
        {{start,delivery,3,1},{1,10}},
        {{start,delivery,3,10},{1,10}},
        {{start,requests,3,50},{1,10}}
    ],

    ?assertEqual(Expected, lists:usort(Rows)),
    swirl_flow:stop(Flow).

%% utils
cleanup() ->
    error_logger:tty(false),
    application:stop(swirl),
    error_logger:tty(true).

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
    #{type => Type, exchange_id => ExchangeId, bidder_id => BidderId}.

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
