

# Module swirl_stream #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-event">event()</a> ###



<pre><code>
event() = [{atom(), <a href="#type-value">value()</a>}]
</code></pre>





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





### <a name="type-value">value()</a> ###



<pre><code>
value() = integer() | float() | binary()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#emit-2">emit/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td></td></tr><tr><td valign="top"><a href="#unregister-1">unregister/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="emit-2"></a>

### emit/2 ###


<pre><code>
emit(StreamName::<a href="#type-stream_name">stream_name()</a>, Event::<a href="#type-event">event()</a>) -&gt; ok
</code></pre>
<br />


<a name="lookup-1"></a>

### lookup/1 ###


<pre><code>
lookup(StreamName::<a href="#type-stream_name">stream_name()</a>) -&gt; [tuple()]
</code></pre>
<br />


<a name="register-2"></a>

### register/2 ###


<pre><code>
register(Flow::<a href="#type-flow">flow()</a>, TableId::<a href="ets.md#type-tab">ets:tab()</a>) -&gt; true
</code></pre>
<br />


<a name="unregister-1"></a>

### unregister/1 ###


<pre><code>
unregister(Flow::<a href="#type-flow">flow()</a>) -&gt; true
</code></pre>
<br />


