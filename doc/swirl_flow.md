

# Module swirl_flow #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `swirl_flow` behaviour.__<br /> Required callback functions: `map/3`, `reduce/3`, `output/4`.

<a name="types"></a>

## Data Types ##




### <a name="type-flow">flow()</a> ###


<pre><code>
flow() = #flow{id = binary(), module = module(), module_vsn = undefined | <a href="#type-module_vsn">module_vsn()</a>, stream_filter = undefined | string(), stream_names = undefined | <a href="#type-stream_names">stream_names()</a>, mapper_window = undefined | pos_integer(), mapper_nodes = undefined | [node()], mapper_opts = <a href="#type-mapper_opts">mapper_opts()</a>, reducer_window = undefined | pos_integer(), reducer_node = node(), reducer_opts = <a href="#type-reducer_opts">reducer_opts()</a>, reducer_skip = undefined | boolean(), output_opts = <a href="#type-output_opts">output_opts()</a>, heartbeat = undefined | pos_integer(), window_sync = undefined | boolean(), started_at = undefined | <a href="uerlang.md#type-timestamp">uerlang:timestamp()</a>, start_node = node()}
</code></pre>




### <a name="type-flow_opts">flow_opts()</a> ###


<pre><code>
flow_opts() = {heartbeat, pos_integer()} | {mapper_opts, <a href="#type-mapper_opts">mapper_opts()</a>} | {mapper_window, pos_integer()} | {output_opts, <a href="#type-output_opts">output_opts()</a>} | {reducer_opts, <a href="#type-reducer_opts">reducer_opts()</a>} | {reducer_skip, boolean()} | {reducer_window, pos_integer()} | {stream_filter, string()} | {stream_names, <a href="#type-stream_names">stream_names()</a>} | {window_sync, boolean()}
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#unregister-1">unregister/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="lookup-1"></a>

### lookup/1 ###

<pre><code>
lookup(FlowId::binary() | <a href="#type-flow">flow()</a>) -&gt; undefined | <a href="#type-flow">flow()</a>
</code></pre>
<br />

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(Flow::<a href="#type-flow">flow()</a>) -&gt; true
</code></pre>
<br />

<a name="start-4"></a>

### start/4 ###

<pre><code>
start(FlowMod::atom(), FlowOpts::[<a href="#type-flow_opts">flow_opts()</a>], MapperNodes::[node()], ReducerNode::node()) -&gt; {ok, <a href="#type-flow">flow()</a>} | {error, flow_mod_undef | {bad_flow_opts, list()}}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Flow::<a href="#type-flow">flow()</a>) -&gt; ok
</code></pre>
<br />

<a name="unregister-1"></a>

### unregister/1 ###

<pre><code>
unregister(Flow::<a href="#type-flow">flow()</a>) -&gt; true
</code></pre>
<br />

