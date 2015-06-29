-module(swirl_ql).
-include("swirl.hrl").
-compile([native]).

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
    compare(Comparator, ?LM(Var, Vars), Value);
evaluate({in, Var, List}, Vars) ->
    lists:member(?LM(Var, Vars), List);
evaluate({notin, Var, List}, Vars) ->
    not lists:member(?LM(Var, Vars), List);
evaluate({in_var, Item, Var}, Vars) ->
    lists:member(Item, ?LM(Var, Vars));
evaluate({notin_var, Item, Var}, Vars) ->
    not lists:member(Item, ?LM(Var, Vars));
evaluate({null, Var}, Vars) ->
    ?LM(Var, Vars) =:= ?NULL;
evaluate({notnull, Var}, Vars) ->
    ?LM(Var, Vars) =/= ?NULL.

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
