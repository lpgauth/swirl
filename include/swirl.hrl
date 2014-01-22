-include_lib("deps/swirl_ql/include/swirl_ql.hrl").

%% macros
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
                     {mapper_opts, term()} |
                     {reducer_flush, pos_integer()} |
                     {reducer_opts, term()}.
-type update() :: {update, tuple(), tuple()}.
