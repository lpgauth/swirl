-module(swirl_utils).
-include("swirl.hrl").
-compile(native).

%% public
-export([
    epoch_ms/0,
    lookup/2,
    lookup/3,
    new_timer/2,
    new_timer/3,
    safe_dict_fetch/2,
    safe_ets_delete/1,
    safe_ets_increment/3,
    update_op/1,
    uuid/0
]).

%% public
epoch_ms() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000000 + Sec * 1000) + trunc(Micro / 1000).

lookup(Key, List) ->
    lookup(Key, List, undefined).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

new_timer(Time, Msg) ->
    new_timer(Time, Msg, self()).

new_timer(Time, Msg, To) ->
    EpochMs = epoch_ms(),
    Delta = EpochMs rem Time,
    TimerRef = erlang:send_after(Time - Delta, To, Msg),
    {EpochMs, TimerRef}.

safe_dict_fetch(Key, Dict) ->
    try dict:fetch(Key, Dict)
    catch
        error:badarg -> undefined
    end.

safe_ets_increment(TableId, Key, UpdateOp) ->
    try ets:update_counter(TableId, Key, UpdateOp)
    catch
        error:badarg ->
            case ets:info(TableId) of
                undefined ->
                    ok;
                _Else ->
                    NumCounters = length(UpdateOp),
                    New = list_to_tuple([Key] ++ [0 || _ <- lists:seq(1, NumCounters)]),
                    ets:insert(TableId, New),
                    ets:update_counter(TableId, Key, UpdateOp)
            end
    end.

safe_ets_delete(TableId) ->
    try ets:delete(TableId)
    catch
        error:badarg ->
            ok
    end.

update_op(Counters) when is_tuple(Counters) ->
    update_op(tuple_to_list(Counters), 2);
update_op(Counters) when is_list(Counters) ->
    update_op(Counters, 2).

uuid() ->
    uuid:get_v1(uuid:new(self(), os)).

%% private
update_op([], _Pos) ->
    [];
update_op([Counter | T], Pos) ->
    [{Pos, Counter} | update_op(T, Pos + 1)].
