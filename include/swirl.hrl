%% macros
-define(DEFAULT_MAPPER_FLUSH, timer:seconds(1)).
-define(DEFAULT_REDUCER_FLUSH, timer:seconds(1)).
-define(DEFAULT_HEARTBEAT, timer:seconds(5)).

-define(TABLE_NAME_FLOWS, swirl_flows).
-define(TABLE_NAME_MAPPERS, swirl_mappers).
-define(TABLE_NAME_REDUCERS, swirl_reducers).

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
                     {reducer_opts, term()} |
                     {heartbeat, pos_integer()}.
-type update() :: {update, tuple(), tuple()}.
-type value() :: integer() | float() | binary().
