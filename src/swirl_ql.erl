-module(swirl_ql).
-include("swirl.hrl").
-compile([native]).

-export([
    evaluate/2,
    parse/1
]).

%% public
-spec evaluate(exp_tree(), [{atom(), value()}]) -> boolean().

evaluate({'and', A, B}, Vars) ->
    evaluate(A, Vars) andalso evaluate(B, Vars);
evaluate({'or', A, B}, Vars) ->
    evaluate(A, Vars) orelse evaluate(B, Vars);
evaluate({'<', Var, Value}, Vars) ->
    lookup(Var, Vars) < Value;
evaluate({'<=', Var, Value}, Vars) ->
    lookup(Var, Vars) =< Value;
evaluate({'=', Var, Value}, Vars) ->
    lookup(Var, Vars) =:= Value;
evaluate({'>=', Var, Value}, Vars) ->
    lookup(Var, Vars) >= Value;
evaluate({'>', Var, Value}, Vars) ->
    lookup(Var, Vars) > Value;
evaluate({'<>', Var, Value}, Vars) ->
    lookup(Var, Vars) =/= Value;
evaluate({in, Var, List}, Vars) ->
    lists:member(lookup(Var, Vars), List);
evaluate({notin, Var, List}, Vars) ->
    not lists:member(lookup(Var, Vars), List);
evaluate({null, Var}, Vars) ->
    lookup(Var, Vars) =:= ?NULL;
evaluate({notnull, Var}, Vars) ->
    lookup(Var, Vars) =/= ?NULL.

-spec parse(string() | binary()) -> {ok, exp_tree()} | {error, term()}.

parse(String) when is_binary(String) ->
    parse(binary_to_list(String));
parse(String) when is_list(String) ->
    case swirl_ql_lexer:string(String) of
        {ok, Tokens, _} ->
            case swirl_ql_parser:parse(Tokens) of
                {ok, ExpTree} -> {ok, ExpTree};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason, _} -> {error, Reason}
    end.

%% private
lookup(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> ?NULL;
        {_, Value} -> Value
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% tests
benchmark_test() ->
    {ok, ExpTree} = parse("exchange_id = 1 AND exchange_seller_id = 181 AND bidder_id IN (1, 5) AND buyer_spend > 150"),

    Vars = [
        {exchange_id, 1},
        {exchange_seller_id, 181},
        {bidder_id, 1},
        {buyer_spend, 200}
    ],

    FunEvaluate = fun() -> evaluate(ExpTree, Vars) end,
    benchmark(evaluate, FunEvaluate, 10000000).

evaluate_test() ->
    % comp predictate
    assert_eval({'=', bidder_id, 1}, [{bidder_id, 1}]),
    assert_not_eval({'=', bidder_id, 1}, [{bidder_id, 2}]),
    assert_eval({'<', price, 100}, [{price, 60}]),
    assert_not_eval({'<', price, 100}, [{price, 160}]),
    assert_eval({'<=', price, 100}, [{price, 100}]),
    assert_not_eval({'<=', price, 100}, [{price, 160}]),
    assert_eval({'>=', price, 100}, [{price, 100}]),
    assert_not_eval({'>=', price, 160}, [{price, 100}]),
    assert_eval({'>', price, 100}, [{price, 160}]),
    assert_not_eval({'>', price, 100}, [{price, 60}]),
    assert_eval({'<>', price, 100}, [{price, 160}]),
    assert_not_eval({'<>', price, 100}, [{price, 100}]),

    % in predictate
    assert_eval({in, exchange_id, [1 , 2]}, [{exchange_id, 2}]),
    assert_not_eval({in, exchange_id, [1 , 2]}, [{exchange_id, 3}]),
    assert_eval({notin, exchange_id, [1 , 2]}, [{exchange_id, 3}]),
    assert_not_eval({notin, exchange_id, [1 , 2]}, [{exchange_id, 2}]),

    % null predictate
    assert_eval({null, exchange_id}, []),
    assert_not_eval({null, exchange_id}, [{exchange_id, 3}]),
    assert_eval({notnull, exchange_id}, [{exchange_id, 3}]),
    assert_not_eval({notnull, exchange_id}, [{exchange_id, ?NULL}]),

    % and
    assert_eval({'and', {'=', bidder_id, 1}, {'=', bidder_id, 1}},
        [{bidder_id, 1}]),
    assert_not_eval({'and', {'=', bidder_id, 1}, {'=', exchange_id, 1}},
        [{bidder_id, 1}, {exchange_id, 2}]),

    % or
    assert_eval({'or', {'=', bidder_id, 2}, {'=', bidder_id, 1}},
        [{bidder_id, 1}]),
    assert_not_eval({'or', {'=', bidder_id, 2}, {'=', bidder_id, 3}},
        [{bidder_id, 1}]).

parse_test() ->
    assert_parse({'=', bidder_id, 1}, "bidder_id = 1"),
    assert_parse({'=', domain, <<"ebay.ca">>}, "domain = 'ebay.ca'"),
    assert_parse({'=', domain, <<"ebay.ca">>}, "domain = \"ebay.ca\""),
    assert_parse({in, exchange_id, [1, 2, 3]}, "exchange_id IN (1, 2, 3)"),
    assert_parse({'and', {'=', bidder_id, 1}, {'or', {notin, exchange_id, [1 , 2]},
      {'=', domain, <<"ebay.ca">>}}}, "bidder_id = 1 AND (exchange_id NOT IN (1, 2) OR domain = 'ebay.ca')").

%% test_utils
assert_eval(ExpTree, Vars) ->
    ?assert(evaluate(ExpTree, Vars)).

assert_not_eval(ExpTree, Vars) ->
    ?assertNot(evaluate(ExpTree, Vars)).

assert_parse(Expected, Expression) ->
    {ok, ExpTree} = parse(Expression),
    ?assertEqual(Expected, ExpTree).

benchmark(Name, Fun, N) ->
    Timestamp = os:timestamp(),
    ok = loop(Fun, N),
    Time = timer:now_diff(os:timestamp(), Timestamp) / N,
    ?debugFmt("~p: ~p microseconds", [Name, Time]).

loop(_, 0) ->
    ok;
loop(Fun, N) ->
    Fun(),
    loop(Fun, N - 1).

-endif.
