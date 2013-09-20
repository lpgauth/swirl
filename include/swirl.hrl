%% macros
-define(NULL, undefined).

-define(DEFAULT_MAPPER_HEARTBEAT, timer:seconds(5)).
-define(DEFAULT_MAPPER_FLUSH, timer:seconds(1)).
-define(DEFAULT_REDUCER_FLUSH, timer:seconds(1)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(L(Key, List), swirl_utils:lookup(Key, List)).
-define(L(Key, List, Default), swirl_utils:lookup(Key, List, Default)).

%% records
-record(period, {
    start_at,
    end_at
}).

%% types
-type event() :: [{atom(), value()}].
-type period() :: #period {}.
-type flow_opts() :: {stream_name, atom()} |
                     {stream_filter, string()} |
                     {mapper_flush, pos_integer()} |
                     {mapper_heartbeat, pos_integer()} |
                     {mapper_opts, term()} |
                     {reducer_flush, pos_integer()} |
                     {reducer_opts, term()}.

-type boolean_op() :: 'and' | 'or'.
-type comparison_op() :: '<' | '<=' | '=' | '>=' | '>' | '<>'.
-type inclusion_op() :: in | notin.
-type null_op() :: null | notnull.
-type value() :: integer() | float() | binary().

-type exp_tree() :: {boolean_op(), exp_tree(), exp_tree()} |
                    {comparison_op(), atom(), value()} |
                    {inclusion_op(), atom(), [value(), ...]} |
                    {null_op(), atom()}.
