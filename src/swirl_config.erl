-module(swirl_config).
-include("swirl.hrl").

-export([
    flows/0,
    mappers/0,
    reducers/0
]).

flows() ->
    swirl_utils:tab2list(?TABLE_NAME_FLOWS).

mappers() ->
    swirl_utils:tab2list(?TABLE_NAME_MAPPERS).

reducers() ->
    swirl_utils:tab2list(?TABLE_NAME_REDUCERS).
