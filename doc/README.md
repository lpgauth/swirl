

# swirl #

__Authors:__ Louis-Philippe Gauthier.

Lightweight distributed stream processor

#### Examples: ####

##### Starting a flow: #####

```
ok = application:start(swirl),
..
FlowMod = swirl_flow_example,
FlowOpts = [
    {stream_names, [delivery]},
    {stream_filter, "exchange_id = 3 AND bidder_id IS NOT NULL"}
],
MapperNodes = [node()],
ReducerNode = node(),
{ok, Flow} = swirl_flow:start(FlowMod, FlowOpts, MapperNodes, ReducerNode),
..
swirl_stream:emit(delivery, [{exchange_id, 1}, {bidder_id, 10}]),
..
ok = swirl_flow:stop(Flow).
```
##### Implementing a flow: #####

```
module(swirl_flow_example).

-behavior(swirl_flow).
-export([
    map/3,
    reduce/3,
    output/4
]).

%% swirl_flow callbacks
map(_StreamName, Event, _MapperOpts) ->
    {{l(type, Event), l(exchange_id, Event), l(bidder_id, Event)}, {1, 10}}.

reduce(_Flow, Row, _ReducerOpts) ->
    Row.

output(_Flow, _Period, Rows, OutputOpts) ->
    case l(send_to , OutputOpts) of
        undefined -> ok;
        Pid -> Pid ! Rows
    end.

%% helpers
l(Key, Event) ->
    swirl_utils:lookup(Key, Event).
```

#### Resource Limitation: ####

configurable via:

```
application:set_env(swirl, mappers_max, 140))
application:set_env(swirl, reducers_max, 200))
```
#### TODO: ####
- node discovery
- boolean expression indexing


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="swirl.md" class="module">swirl</a></td></tr>
<tr><td><a href="swirl_code_server.md" class="module">swirl_code_server</a></td></tr>
<tr><td><a href="swirl_config.md" class="module">swirl_config</a></td></tr>
<tr><td><a href="swirl_ets_manager.md" class="module">swirl_ets_manager</a></td></tr>
<tr><td><a href="swirl_flow.md" class="module">swirl_flow</a></td></tr>
<tr><td><a href="swirl_mapper.md" class="module">swirl_mapper</a></td></tr>
<tr><td><a href="swirl_reducer.md" class="module">swirl_reducer</a></td></tr>
<tr><td><a href="swirl_stream.md" class="module">swirl_stream</a></td></tr>
<tr><td><a href="swirl_sup.md" class="module">swirl_sup</a></td></tr>
<tr><td><a href="swirl_tracker.md" class="module">swirl_tracker</a></td></tr>
<tr><td><a href="swirl_utils.md" class="module">swirl_utils</a></td></tr></table>

