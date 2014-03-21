-include_lib("swirl_ql/include/swirl_ql.hrl").

%% macros
-define(DEFAULT_MAPPER_FLUSH, timer:seconds(1)).
-define(DEFAULT_MAPPERS_MAX, 100).
-define(DEFAULT_REDUCER_FLUSH, timer:seconds(1)).
-define(DEFAULT_REDUCERS_MAX, 100).
-define(DEFAULT_HEARTBEAT, timer:seconds(5)).

-define(TABLE_NAME_CODE_SERVER, swirl_code_server).
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
    module_vsn    :: pos_integer(),
    stream_name   :: atom(),
    stream_filter :: string(),
    mapper_flush  :: pos_integer(),
    mapper_nodes  :: [node()],
    mapper_opts   :: term(),
    reducer_flush :: pos_integer(),
    reducer_node  :: node(),
    reducer_opts  :: term(),
    heartbeat     :: pos_integer(),
    started_at    :: erlang:timestamp(),
    start_node    :: node()
}).

-record(stream, {
    flow_id       :: binary(),
    flow_mod      :: module(),
    flow_mod_vsn  :: md5(),
    start_node    :: node(),
    exp_tree      :: exp_tree(),
    mapper_opts   :: term(),
    table_id      :: ets:tab()
}).

-record(period, {
    start_at      :: pos_integer(),
    end_at        :: pos_integer()
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
-type md5()       :: pos_integer().
-type period()    :: #period {}.
-type stream()    :: #stream {}.
-type update()    :: {update, tuple(), tuple()}.
