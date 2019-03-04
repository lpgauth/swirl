-module(swirl_ql).
-include("swirl.hrl").

-export([
    evaluate/2,
    parse/1
]).

%% public
-spec evaluate(exp_tree(), event()) -> boolean().
evaluate({'and', A, B}, Vars) ->
    evaluate(A, Vars) andalso evaluate(B, Vars);
evaluate({'or', A, B}, Vars) ->
    evaluate(A, Vars) orelse evaluate(B, Vars);
evaluate({comp, Comparator, Var, Value}, Vars) ->
    compare(Comparator, ?L(Var, Vars), Value);
evaluate({in, Var, List}, Vars) ->
    lists:member(?L(Var, Vars), List);
evaluate({notin, Var, List}, Vars) ->
    not lists:member(?L(Var, Vars), List);
evaluate({in_var, Item, Var}, Vars) ->
    lists:member(Item, ?L(Var, Vars));
evaluate({notin_var, Item, Var}, Vars) ->
    not lists:member(Item, ?L(Var, Vars));
evaluate({null, Var}, Vars) ->
    ?L(Var, Vars) =:= ?NULL;
evaluate({notnull, Var}, Vars) ->
    ?L(Var, Vars) =/= ?NULL.

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
compare(_Comp, undefined, _B) ->
    false;
compare('<', A, B) when is_number(A) and
                        is_number(B) ->
    A < B;
compare('<=', A, B) when is_number(A) and
                         is_number(B) ->
    A =< B;
compare('=', A, B) when is_number(A) or
                        is_binary(A) and
                        is_number(B) or
                        is_binary(B) ->
    A == B;
compare('>=', A, B) when is_number(A) and
                         is_number(B) ->
    A >= B;
compare('>', A, B) when is_number(A) and
                        is_number(B) ->
    A > B;
compare('<>', A, B) when is_number(A) or
                         is_binary(A) and
                         is_number(B) or
                         is_binary(B) ->
    A /= B.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% tests
benchmark_test() ->
    {ok, ExpTree} = parse("exchange_id = 1 AND exchange_seller_id = 181 AND bidder_id IN (1, 5) AND buyer_spend > 150"),

    Vars = # {
        exchange_id => 1,
        exchange_seller_id => 181,
        bidder_id => 1,
        buyer_spend => 200
    },

    FunEvaluate = fun() -> evaluate(ExpTree, Vars) end,
    benchmark(evaluate, FunEvaluate, 100000).

evaluate_test() ->
    % comp predictate
    assert_eval({comp, '=', bidder_id, 1}, #{bidder_id => 1}),
    assert_not_eval({comp, '=', bidder_id, 1}, #{bidder_id => 2}),
    assert_eval({comp, '<', price, 100}, #{price => 60}),
    assert_not_eval({comp, '<', price, 100}, #{price => 160}),
    assert_eval({comp, '<=', price, 100}, #{price => 100}),
    assert_not_eval({comp, '<=', price, 100}, #{price => 160}),
    assert_eval({comp, '>=', price, 100}, #{price => 100}),
    assert_not_eval({comp, '>=', price, 160}, #{price => 100}),
    assert_eval({comp, '>', price, 100}, #{price => 160}),
    assert_not_eval({comp, '>', price, 100}, #{price => 60}),
    assert_eval({comp, '<>', price, 100}, #{price => 160}),
    assert_not_eval({comp, '<>', price, 100}, #{price => 100}),

    % in predictate
    assert_eval({in, exchange_id, [1 , 2]}, #{exchange_id => 2}),
    assert_not_eval({in, exchange_id, [1 , 2]}, #{exchange_id => 3}),
    assert_eval({notin, exchange_id, [1 , 2]}, #{exchange_id => 3}),
    assert_not_eval({notin, exchange_id, [1 , 2]}, #{exchange_id => 2}),
    assert_eval({in_var, 54, segment_ids}, #{segment_ids => [12, 54]}),
    assert_not_eval({in_var, 54, segment_ids}, #{segment_ids => [12]}),
    assert_eval({notin_var, 54, segment_ids}, #{segment_ids => [12]}),
    assert_not_eval({notin_var, 54, segment_ids}, #{segment_ids => [12, 54]}),

    % null predictate
    assert_eval({null, exchange_id}, #{}),
    assert_not_eval({null, exchange_id}, #{exchange_id => 3}),
    assert_eval({notnull, exchange_id}, #{exchange_id => 3}),
    assert_not_eval({notnull, exchange_id}, #{exchange_id => ?NULL}),

    % and
    assert_eval({'and', {comp, '=', bidder_id, 1}, {comp, '=', bidder_id, 1}},
        #{bidder_id => 1}),
    assert_not_eval({'and', {comp, '=', bidder_id, 1}, {comp, '=', exchange_id, 1}},
        #{bidder_id => 1, exchange_id => 2}),

    % or
    assert_eval({'or', {comp, '=', bidder_id, 2}, {comp, '=', bidder_id, 1}},
        #{bidder_id => 1}),
    assert_not_eval({'or', {comp, '=', bidder_id, 2}, {comp, '=', bidder_id, 3}},
        #{bidder_id => 1}).

parse_test() ->
    assert_parse({comp, '=', bidder_id, 1}, "bidder_id = 1"),
    assert_parse({comp, '=', domain, <<"ebay.ca">>}, "domain = 'ebay.ca'"),
    assert_parse({comp, '=', domain, <<"ebay.ca">>}, "domain = \"ebay.ca\""),
    assert_parse({in, exchange_id, [1, 2, 3]}, "exchange_id IN (1, 2, 3)"),
    assert_parse({in_var, 4, segment_ids}, "4 IN segment_ids"),
    assert_parse({notin_var, 8, segment_ids}, "8 NOT IN segment_ids"),
    assert_parse({'and', {comp, '=', bidder_id, 1}, {'or', {notin, exchange_id, [1 , 2]},
      {comp, '=', domain, <<"ebay.ca">>}}}, "bidder_id = 1 AND (exchange_id NOT IN (1, 2) OR domain = 'ebay.ca')").

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
