

# swirl #

__Authors:__ Louis-Philippe Gauthier.

Lightweight Distributed Stream Processor

[![Build Status](https://travis-ci.org/lpgauth/swirl.svg?branch=master)](https://travis-ci.org/lpgauth/swirl)

#### Requirements: ####
- Erlang 17.0+

#### Examples: ####

##### Starting a flow: #####

```erlang
ok = application:start(swirl),

FlowMod = swirl_flow_example,
FlowOpts = [
    {stream_names, [delivery]},
    {stream_filter, "exchange_id = 3 AND bidder_id IS NOT NULL"}
],
MapperNodes = [node()],
ReducerNode = node(),
{ok, Flow} = swirl_flow:start(FlowMod, FlowOpts, MapperNodes, ReducerNode),

swirl_stream:emit(delivery, #{exchange_id => 1, bidder_id => 10}),

ok = swirl_flow:stop(Flow)
```

##### Implementing a flow: #####

```erlang
-module(swirl_flow_example).
-include_lib("swirl/include/swirl.hrl").

-behavior(swirl_flow).
-export([
    map/3,
    reduce/3,
    output/4
]).

%% swirl_flow callbacks
map(StreamName, Event, _MapperOpts) ->
    {{?LM(type, Event), StreamName, ?LM(exchange_id, Event), ?LM(bidder_id, Event)}, {1, 10}}.

reduce(_Flow, Row, _ReducerOpts) ->
    Row.

output(_Flow, _Period, Rows, OutputOpts) ->
    case ?L(send_to , OutputOpts) of
        undefined -> ok;
        Pid -> Pid ! Rows
    end.
```

#### Stream Filter: ####
##### Examples: #####

```erlang

exchange_id = 3 AND bidder_id IS NOT NULL
flight_id in (10, 12, 23) OR tag_id = 20
buyer_id notnull AND seller_id > 103
```
Variables:

```

atom()
```
Values:

```

integer() | float() | binary()
```
Boolean Operators:

```

'and' | 'or'
```
Comparison Operators:

```

'<' | '<=' | '=' | '>=' | '>' | '<>'
```
Inclusion Operators:

```

in | notin
```
Null Operators:

```

null | notnull
```

#### Resource Limitation: ####

configurable via:

```erlang
application:set_env(swirl, mappers_max, 140)
application:set_env(swirl, reducers_max, 200)
```

#### TODO: ####
- node discovery
- boolean expression indexing


#### Modules: ####


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl.md" class="module">swirl</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_code_server.md" class="module">swirl_code_server</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_config.md" class="module">swirl_config</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_ets_manager.md" class="module">swirl_ets_manager</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_flow.md" class="module">swirl_flow</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_mapper.md" class="module">swirl_mapper</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_ql.md" class="module">swirl_ql</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_ql_lexer.md" class="module">swirl_ql_lexer</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_ql_parser.md" class="module">swirl_ql_parser</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_reducer.md" class="module">swirl_reducer</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_stream.md" class="module">swirl_stream</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_sup.md" class="module">swirl_sup</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_tracker.md" class="module">swirl_tracker</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/swirl/blob/dev/doc/swirl_utils.md" class="module">swirl_utils</a></td></tr></table>

