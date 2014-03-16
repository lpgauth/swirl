-module(swirl_config).
-include("swirl.hrl").

%% public
-export([
    flows/0,
    mappers/0,
    mappers_count/0,
    mappers_max/0,
    reducers/0,
    reducers_count/0,
    reducers_max/0
]).

%% public
flows() ->
    swirl_utils:tab2list(?TABLE_NAME_FLOWS).

mappers() ->
    swirl_utils:tab2list(?TABLE_NAME_MAPPERS).

mappers_count() ->
    select_count_all(?TABLE_NAME_MAPPERS).

mappers_max() ->
    application:get_env(swirl, mappers_max, ?DEFAULT_MAPPERS_MAX).

reducers() ->
    swirl_utils:tab2list(?TABLE_NAME_REDUCERS).

reducers_count() ->
    select_count_all(?TABLE_NAME_REDUCERS).

reducers_max() ->
    application:get_env(swirl, reducers_max, ?DEFAULT_REDUCERS_MAX).

%% private
select_count_all(TableId) ->
    ets:select_count(TableId, [{'_', [], [true]}]).
