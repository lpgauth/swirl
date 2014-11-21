%% macros
-define(DEFAULT_HEARTBEAT, timer:seconds(5)).
-define(DEFAULT_MAPPERS_MAX, 100).
-define(DEFAULT_MAPPER_WINDOW, timer:seconds(1)).
-define(DEFAULT_REDUCERS_MAX, 100).
-define(DEFAULT_REDUCER_SKIP, false).
-define(DEFAULT_REDUCER_WINDOW, timer:seconds(1)).
-define(DEFAULT_WINDOW_SYNC, false).

-define(TABLE_NAME_CODE_SERVER, swirl_code_server).
-define(TABLE_NAME_FLOWS, swirl_flows).
-define(TABLE_NAME_MAPPERS, swirl_mappers).
-define(TABLE_NAME_REDUCERS, swirl_reducers).
-define(TABLE_NAME_STREAMS, swirl_streams).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(L(Key, List), swirl_utils:lookup(Key, List)).
-define(L(Key, List, Default), swirl_utils:lookup(Key, List, Default)).
-define(LM(Key, List), swirl_utils:lookup(Key, List)).
-define(LM(Key, List, Default), swirl_utils:lookup(Key, List, Default)).
-define(NULL, undefined).

%% records
-record(flow, {
    id             :: binary(),
    module         :: module(),
    module_vsn     :: module_vsn(),
    stream_filter  :: string(),
    stream_names   :: stream_names(),
    mapper_window  :: pos_integer(),
    mapper_nodes   :: [node()],
    mapper_opts    :: mapper_opts(),
    reducer_window :: pos_integer(),
    reducer_node   :: node(),
    reducer_opts   :: reducer_opts(),
    reducer_skip   :: boolean(),
    output_opts    :: output_opts(),
    heartbeat      :: pos_integer(),
    window_sync    :: boolean(),
    started_at     :: erlang:timestamp(),
    start_node     :: node()
}).

-record(stream, {
    flow_id      :: binary(),
    flow_mod     :: module(),
    flow_mod_vsn :: module_vsn(),
    start_node   :: node(),
    exp_tree     :: exp_tree(),
    mapper_opts  :: mapper_opts(),
    table_id     :: ets:tab()
}).

-record(period, {
    start_at :: pos_integer(),
    end_at   :: pos_integer()
}).

%% types
-type flow()         :: #flow {}.
-type flow_opts()    :: {heartbeat, pos_integer()} |
                        {mapper_opts, mapper_opts()} |
                        {mapper_window, pos_integer()} |
                        {output_opts, output_opts()} |
                        {reducer_opts, reducer_opts()} |
                        {reducer_skip, boolean()} |
                        {reducer_window, pos_integer()} |
                        {stream_filter, string()} |
                        {stream_names, stream_names()} |
                        {window_sync, boolean()}.
-type mapper_opts()  :: term().
-type module_vsn()   :: pos_integer().
-type output_opts()  :: term().
-type period()       :: #period {}.
-type reducer_opts() :: term().
-type row()          :: tuple().
-type stream()       :: #stream {}.
-type stream_name()  :: atom().
-type stream_names() :: [stream_name()].
-type update()       :: {tuple(), tuple()}.

-type event()        :: map().

-type boolean_op() :: 'and' | 'or'.
-type comparison_op() :: '<' | '<=' | '=' | '>=' | '>' | '<>'.
-type inclusion_op() :: in | notin.
-type null_op() :: null | notnull.
-type variable() :: atom().
-type value() :: integer() | float() | binary().

-type exp_tree() :: {boolean_op(), exp_tree(), exp_tree()} |
                    {comparison_op(), variable(), value()} |
                    {inclusion_op(), variable(), [value(), ...]} |
                    {null_op(), variable()}.
