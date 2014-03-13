-module(swirl_utils).
-include("swirl.hrl").

%% public
-export([
    lookup/2,
    lookup/3,
    new_timer/2,
    new_timer/3,
    safe_dict_fetch/2,
    safe_ets_delete/1,
    safe_ets_increment/3,
    safe_ets_lookup_element/2,
    tab2list/1,
    unix_timestamp_ms/0,
    update_op/1,
    uuid/0
]).

%% public
lookup(Key, List) ->
    lookup(Key, List, undefined).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

maybe_tuple_to_list(Tuple) when is_tuple(Tuple) ->
    tuple_to_list(Tuple);
maybe_tuple_to_list(List) when is_list(List) ->
    List.

new_timer(Time, Msg) ->
    new_timer(Time, Msg, self()).

new_timer(Time, Msg, To) ->
    Timestamp = unix_timestamp_ms(),
    Delta = Timestamp rem Time,
    TimerRef = erlang:send_after(Time - Delta, To, Msg),
    {Timestamp, TimerRef}.

safe_dict_fetch(Key, Dict) ->
    try dict:fetch(Key, Dict)
    catch
        error:badarg -> undefined
    end.

safe_ets_delete(TableId) ->
    try ets:delete(TableId)
    catch
        error:badarg ->
            ok
    end.

safe_ets_increment(TableId, Key, Counters) ->
    UpdateOp = swirl_utils:update_op(Counters),
    try ets:update_counter(TableId, Key, UpdateOp)
    catch
        error:badarg ->
            safe_ets_insert(TableId, Key, Counters)
    end.

safe_ets_insert(TableId, Key, Counters) ->
    try
        New = list_to_tuple([Key] ++ maybe_tuple_to_list(Counters)),
        ets:insert(TableId, New)
    catch
        error:badarg ->
            ok
    end.

safe_ets_lookup_element(TableId, Key) ->
    try
         ets:lookup_element(TableId, Key, 2)
    catch
        error:badarg ->
            undefined
    end.

tab2list(Tid) ->
    lists:append(match_all(ets:match_object(Tid, '_', 500))).

unix_timestamp_ms() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000000 + Sec * 1000) + trunc(Micro / 1000).

update_op(Counters) when is_tuple(Counters) ->
    update_op(tuple_to_list(Counters), 2);
update_op(Counters) when is_list(Counters) ->
    update_op(Counters, 2).

uuid() ->
    uuid:get_v1(uuid:new(self(), os)).

%% private
match_all('$end_of_table') ->
    [];
match_all({Match, Continuation}) ->
    [Match | match_all(ets:match_object(Continuation))].

update_op([], _Pos) ->
    [];
update_op([Counter | T], Pos) ->
    [{Pos, Counter} | update_op(T, Pos + 1)].
