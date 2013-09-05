-module(swirl_stream).
-include("swirl.hrl").
-compile([native]).

%% public
-export([
    emit/2
]).

%% public
-spec emit(atom(), event()) -> ok.
emit(StreamName, Event) ->
    Flows = swirl_flow:lookup(StreamName),
    evaluate(StreamName, Event, Flows).

%% private
evaluate(_StreamName, _Event, []) ->
    ok;
evaluate(StreamName, Event, [{undefined, FlowMod, MapperOpts, TableId} | T]) ->
    swirl_mapper:map(FlowMod, StreamName, Event, MapperOpts, TableId),
    evaluate(StreamName, Event, T);
evaluate(StreamName, Event, [{ExpTree, FlowMod, MapperOpts, TableId} | T]) ->
    case swirl_ql:evaluate(ExpTree, Event) of
        true ->
            swirl_mapper:map(FlowMod, StreamName, Event, MapperOpts, TableId);
        false ->
            ok
    end,
    evaluate(StreamName, Event, T).
