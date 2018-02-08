-module(swirl_ql_tests).
-include("test.hrl").

-compile(export_all).

% runners
swirl_test_() ->
    {inparallel, [
        ?T(test_evaluate),
        ?T(test_parse)
    ]}.

% tests
test_evaluate() ->
    % comp predictate
    assert_eval({comp, '=', bidder_id, 1}, [{bidder_id, 1}]),
    assert_not_eval({comp, '=', bidder_id, 1}, [{bidder_id, 2}]),
    assert_eval({comp, '<', price, 100}, [{price, 60}]),
    assert_not_eval({comp, '<', price, 100}, [{price, 160}]),
    assert_eval({comp, '<=', price, 100}, [{price, 100}]),
    assert_not_eval({comp, '<=', price, 100}, [{price, 160}]),
    assert_eval({comp, '>=', price, 100}, [{price, 100}]),
    assert_not_eval({comp, '>=', price, 160}, [{price, 100}]),
    assert_eval({comp, '>', price, 100}, [{price, 160}]),
    assert_not_eval({comp, '>', price, 100}, [{price, 60}]),
    assert_eval({comp, '<>', price, 100}, [{price, 160}]),
    assert_not_eval({comp, '<>', price, 100}, [{price, 100}]),

    % in predictate
    assert_eval({in, exchange_id, [1 , 2]}, [{exchange_id, 2}]),
    assert_not_eval({in, exchange_id, [1 , 2]}, [{exchange_id, 3}]),
    assert_eval({notin, exchange_id, [1 , 2]}, [{exchange_id, 3}]),
    assert_not_eval({notin, exchange_id, [1 , 2]}, [{exchange_id, 2}]),
    assert_eval({in_var, 54, segment_ids}, [{segment_ids, [12, 54]}]),
    assert_not_eval({in_var, 54, segment_ids}, [{segment_ids, [12]}]),
    assert_eval({notin_var, 54, segment_ids}, [{segment_ids, [12]}]),
    assert_not_eval({notin_var, 54, segment_ids}, [{segment_ids, [12, 54]}]),

    % null predictate
    assert_eval({null, exchange_id}, []),
    assert_not_eval({null, exchange_id}, [{exchange_id, 3}]),
    assert_eval({notnull, exchange_id}, [{exchange_id, 3}]),
    assert_not_eval({notnull, exchange_id}, [{exchange_id, ?NULL}]),

    % and
    assert_eval({'and', {comp, '=', bidder_id, 1}, {comp, '=', bidder_id, 1}},
        [{bidder_id, 1}]),
    assert_not_eval({'and', {comp, '=', bidder_id, 1},
        {comp, '=', exchange_id, 1}}, [{bidder_id, 1}, {exchange_id, 2}]),

    % or
    assert_eval({'or', {comp, '=', bidder_id, 2}, {comp, '=', bidder_id, 1}},
        [{bidder_id, 1}]),
    assert_not_eval({'or', {comp, '=', bidder_id, 2},
        {comp, '=', bidder_id, 3}}, [{bidder_id, 1}]).

test_parse() ->
    assert_parse({comp, '=', bidder_id, 1}, "bidder_id = 1"),
    assert_parse({comp, '=', domain, <<"ebay.ca">>}, "domain = 'ebay.ca'"),
    assert_parse({comp, '=', domain, <<"ebay.ca">>}, "domain = \"ebay.ca\""),
    assert_parse({in, exchange_id, [1, 2, 3]}, "exchange_id IN (1, 2, 3)"),
    assert_parse({in_var, 4, segment_ids}, "4 IN segment_ids"),
    assert_parse({notin_var, 8, segment_ids}, "8 NOT IN segment_ids"),
    assert_parse({'and', {comp, '=', bidder_id, 1},
        {'or', {notin, exchange_id, [1 , 2]},
        {comp, '=', domain, <<"ebay.ca">>}}},
        "bidder_id = 1 AND (exchange_id NOT IN (1, 2) OR domain = 'ebay.ca')").

%% test_utils
assert_eval(ExpTree, Vars) ->
    ?assert(swirl_ql:evaluate(ExpTree, Vars)).

assert_not_eval(ExpTree, Vars) ->
    ?assertNot(swirl_ql:evaluate(ExpTree, Vars)).

assert_parse(Expected, Expression) ->
    {ok, ExpTree} = swirl_ql:parse(Expression),
    ?assertEqual(Expected, ExpTree).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
