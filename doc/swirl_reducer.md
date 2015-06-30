

# Module swirl_reducer #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-flow">flow()</a> ###


<pre><code>
flow() = #flow{id = undefined | binary(), module = undefined | module(), module_vsn = undefined | <a href="#type-module_vsn">module_vsn()</a>, stream_filter = undefined | string(), stream_names = undefined | <a href="#type-stream_names">stream_names()</a>, mapper_window = undefined | pos_integer(), mapper_nodes = undefined | [node()], mapper_opts = undefined | <a href="#type-mapper_opts">mapper_opts()</a>, reducer_window = undefined | pos_integer(), reducer_node = undefined | node(), reducer_opts = undefined | <a href="#type-reducer_opts">reducer_opts()</a>, reducer_skip = undefined | boolean(), output_opts = undefined | <a href="#type-output_opts">output_opts()</a>, heartbeat = undefined | pos_integer(), window_sync = undefined | boolean(), started_at = undefined | <a href="erlang.md#type-timestamp">erlang:timestamp()</a>, start_node = undefined | node()}
</code></pre>




### <a name="type-mapper_opts">mapper_opts()</a> ###


<pre><code>
mapper_opts() = term()
</code></pre>




### <a name="type-module_vsn">module_vsn()</a> ###


<pre><code>
module_vsn() = pos_integer()
</code></pre>




### <a name="type-output_opts">output_opts()</a> ###


<pre><code>
output_opts() = term()
</code></pre>




### <a name="type-reducer_opts">reducer_opts()</a> ###


<pre><code>
reducer_opts() = term()
</code></pre>




### <a name="type-stream_name">stream_name()</a> ###


<pre><code>
stream_name() = atom()
</code></pre>




### <a name="type-stream_names">stream_names()</a> ###


<pre><code>
stream_names() = [<a href="#type-stream_name">stream_name()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#unregister-1">unregister/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Msg, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(Flow) -> any()`

<a name="lookup-1"></a>

### lookup/1 ###

<pre><code>
lookup(FlowId::binary() | <a href="#type-flow">flow()</a>) -&gt; undefined | pid()
</code></pre>
<br />

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(Flow::<a href="#type-flow">flow()</a>) -&gt; true
</code></pre>
<br />

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Flow::<a href="#type-flow">flow()</a>) -&gt; {ok, pid()} | {error, reducers_max}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="unregister-1"></a>

### unregister/1 ###

<pre><code>
unregister(Flow::<a href="#type-flow">flow()</a>) -&gt; true
</code></pre>
<br />

