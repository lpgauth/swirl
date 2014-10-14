-module(swirl_config).
-include("swirl.hrl").

%% public
-export([
    flows/0,
    flows_count/0,
    mappers/0,
    mappers_count/0,
    mappers_max/0,
    reducers/0,
    reducers_count/0,
    reducers_max/0
]).

%% public
-spec flows() -> list(tuple()).
flows() ->
    swirl_utils:tab2list(?TABLE_NAME_FLOWS).

-spec flows_count() -> non_neg_integer().
flows_count() ->
    select_count_all(?TABLE_NAME_FLOWS).

-spec mappers() -> list(tuple()).
mappers() ->
    swirl_utils:tab2list(?TABLE_NAME_MAPPERS).

-spec mappers_count() -> non_neg_integer().
mappers_count() ->
    select_count_all(?TABLE_NAME_MAPPERS).

-spec mappers_max() -> non_neg_integer().
mappers_max() ->
    application:get_env(swirl, mappers_max, ?DEFAULT_MAPPERS_MAX).

-spec reducers() -> list(tuple()).
reducers() ->
    swirl_utils:tab2list(?TABLE_NAME_REDUCERS).

-spec reducers_count() -> non_neg_integer().
reducers_count() ->
    select_count_all(?TABLE_NAME_REDUCERS).

-spec reducers_max() -> non_neg_integer().
reducers_max() ->
    application:get_env(swirl, reducers_max, ?DEFAULT_REDUCERS_MAX).

%% private
select_count_all(TableId) ->
    ets:select_count(TableId, [{'_', [], [true]}]).
