-module(swirl_benchmarks).

-export([
    all/0
]).

%% public
all() ->
    emit(),
    evaluation().

emit() ->
    error_logger:tty(false),
    application:start(swirl),
    error_logger:tty(true),
    Flows = [new_flow() || _ <- lists:seq(1, 100)],
    timer:sleep(timer:seconds(1)),
    FunEmit = fun() -> swirl_stream:emit(video, random_event()) end,
    benchmark(emit, FunEmit, 10000),
    [swirl_flow:stop(Flow) || Flow <- Flows],
    error_logger:tty(false),
    application:stop(swirl),
    error_logger:tty(true).

evaluation() ->
    {ok, ExpTree} = swirl_ql:parse("exchange_id = 1 AND exchange_seller_id = 181 AND bidder_id IN (1, 5) AND buyer_spend > 150"),

    Vars = [
        {exchange_id, 1},
        {exchange_seller_id, 181},
        {bidder_id, 1},
        {buyer_spend, 200}
    ],

    FunEvaluate = fun() -> swirl_ql:evaluate(ExpTree, Vars) end,
    benchmark(evaluate, FunEvaluate, 100000).

%% private
benchmark(Name, Fun, N) ->
    Timestamp = os:timestamp(),
    ok = loop(Fun, N),
    Time = timer:now_diff(os:timestamp(), Timestamp) / N,
    io:format("~p: ~p microseconds~n", [Name, Time]).

loop(_, 0) -> ok;
loop(Fun, N) ->
    Fun(),
    loop(Fun, N - 1).

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
