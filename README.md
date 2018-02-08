# swirl

High Performance Erlang Stream Processor

[![Build Status](https://travis-ci.org/lpgauth/swirl.svg?branch=master)](https://travis-ci.org/lpgauth/swirl)

### Requirements

* Erlang 16.0 +

### Environment variables

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>mappers_max</td>
    <td>pos_integer()</td>
    <td>100</td>
    <td>maximum number of mappers</td>
  </tr>
  <tr>
    <td>reducers_max</td>
    <td>pos_integer()</td>
    <td>100</td>
    <td>maximum number of reducers</td>
  </tr>
</table>

## Examples

#### Starting a flow

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

StreamName = delivery,
Event = #{exchange_id => 1, bidder_id => 10},

swirl_stream:emit(StreamName, Event),

ok = swirl_flow:stop(Flow)
```

#### Implementing a flow

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
    Type = ?L(type, Event),
    ExchangeId = ?L(exchange_id, Event),
    BidderId = ?L(bidder_id, Event),

    Key = {Type, StreamName, ExchangeId, BidderId},
    CounterIncrements = {1, 10},

    {Key, CounterIncrements}.

reduce(_Flow, Row, _ReducerOpts) ->
    Row.

output(_Flow, _Period, Rows, OutputOpts) ->
    %% do something with the output
    io:format("rows: ~p~n", [Rows]),
```

#### Stream Filter

```erlang
exchange_id = 3 AND bidder_id IS NOT NULL
flight_id in (10, 12, 23) OR tag_id = 20
buyer_id notnull AND seller_id > 103
```

#### Swirl QL

variables:

```
atom()
```
values:

```
integer() | float() | binary()
```
boolean operators:

```
'and' | 'or'
```
comparison operators:

```
'<' | '<=' | '=' | '>=' | '>' | '<>'
```
inclusion operators:

```
in | notin
```
null operators:

```
null | notnull
```

## TODO
* node discovery
* boolean expression indexing

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2013-2016 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```
