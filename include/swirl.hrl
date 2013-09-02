%% macros
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(L(Key, List), swirl_utils:lookup(Key, List)).
-define(L(Key, List, Default), swirl_utils:lookup(Key, List, Default)).

-define(DEFAULT_FLUSH, timer:seconds(1)).
-define(NULL, undefined).

%% types
-type event() :: [{atom(), value()}].
-type mapper_opts() :: {stream_name, atom()} | {stream_filter, string()}.

-type boolean_op() :: 'and' | 'or'.
-type comparison_op() :: '<' | '<=' | '=' | '>=' | '>' | '<>'.
-type inclusion_op() :: in | notin.
-type null_op() :: null | notnull.
-type value() :: integer() | float() | binary().

-type exp_tree() :: {boolean_op(), exp_tree(), exp_tree()} |
                    {comparison_op(), atom(), value()} |
                    {inclusion_op(), atom(), [value(), ...]} |
                    {null_op(), atom()}.
