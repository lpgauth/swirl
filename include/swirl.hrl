-include_lib("swirl_ql/include/swirl_ql.hrl").

%% macros
-define(DEFAULT_MAPPER_FLUSH, timer:seconds(1)).
-define(DEFAULT_MAPPERS_MAX, 100).
-define(DEFAULT_REDUCER_FLUSH, timer:seconds(1)).
-define(DEFAULT_REDUCERS_MAX, 100).
-define(DEFAULT_HEARTBEAT, timer:seconds(5)).

-define(TABLE_NAME_FLOWS, swirl_flows).
-define(TABLE_NAME_MAPPERS, swirl_mappers).
-define(TABLE_NAME_REDUCERS, swirl_reducers).
-define(TABLE_NAME_STREAMS, swirl_streams).


-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(L(Key, List), swirl_utils:lookup(Key, List)).
-define(L(Key, List, Default), swirl_utils:lookup(Key, List, Default)).

%% records
-record(flow, {
    id            :: binary(),
    module        :: module(),
    heartbeat     :: pos_integer(),
    mapper_flush  :: pos_integer(),
    mapper_nodes  :: [node()],
    mapper_opts   :: term(),
    reducer_flush :: pos_integer(),
    reducer_node  :: node(),
    reducer_opts  :: term(),
    stream_filter :: string(),
    stream_name   :: atom(),
    timestamp     :: erlang:timestamp()
}).

-record(stream, {
    flow_id       :: binary(),
    exp_tree      :: exp_tree(),
    flow_mod      :: module(),
    mapper_opts   :: term(),
    table_id      :: ets:tab(),
    reducer_node  :: node()
}).

-record(period, {
    start_at,
    end_at
}).

%% types
-type event()     :: [{atom(), value()}].
-type flow()      :: #flow {}.
-type flow_opts() :: {stream_name, atom()} |
                     {stream_filter, string()} |
                     {mapper_flush, pos_integer()} |
                     {mapper_opts, term()} |
                     {reducer_flush, pos_integer()} |
                     {reducer_opts, term()} |
                     {heartbeat, pos_integer()}.
-type period()    :: #period {}.
-type stream()    :: #stream {}.
-type update()    :: {update, tuple(), tuple()}.
