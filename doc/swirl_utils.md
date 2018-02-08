

# Module swirl_utils #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#new_timer-2">new_timer/2</a></td><td></td></tr><tr><td valign="top"><a href="#new_timer-3">new_timer/3</a></td><td></td></tr><tr><td valign="top"><a href="#proplist_to_record-2">proplist_to_record/2</a></td><td></td></tr><tr><td valign="top"><a href="#record_to_proplist-1">record_to_proplist/1</a></td><td></td></tr><tr><td valign="top"><a href="#safe_dict_fetch-2">safe_dict_fetch/2</a></td><td></td></tr><tr><td valign="top"><a href="#safe_ets_delete-1">safe_ets_delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#safe_ets_increment-3">safe_ets_increment/3</a></td><td></td></tr><tr><td valign="top"><a href="#safe_ets_lookup_element-2">safe_ets_lookup_element/2</a></td><td></td></tr><tr><td valign="top"><a href="#tab2list-1">tab2list/1</a></td><td></td></tr><tr><td valign="top"><a href="#unix_tstamp_ms-0">unix_tstamp_ms/0</a></td><td></td></tr><tr><td valign="top"><a href="#update_op-1">update_op/1</a></td><td></td></tr><tr><td valign="top"><a href="#uuid-0">uuid/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="lookup-2"></a>

### lookup/2 ###

<pre><code>
lookup(Key::term(), List::[{term(), term()}]) -&gt; term()
</code></pre>
<br />

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(Key::term(), List::[{term(), term()}], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="new_timer-2"></a>

### new_timer/2 ###

<pre><code>
new_timer(Time::pos_integer(), Msg::term()) -&gt; <a href="erlang.md#type-reference">erlang:reference()</a>
</code></pre>
<br />

<a name="new_timer-3"></a>

### new_timer/3 ###

<pre><code>
new_timer(Time::pos_integer(), Msg::term(), X3::boolean()) -&gt; <a href="erlang.md#type-reference">erlang:reference()</a>
</code></pre>
<br />

<a name="proplist_to_record-2"></a>

### proplist_to_record/2 ###

<pre><code>
proplist_to_record(Proplist::[{atom(), term()}], Record::atom()) -&gt; tuple()
</code></pre>
<br />

<a name="record_to_proplist-1"></a>

### record_to_proplist/1 ###

`record_to_proplist(Flow) -> any()`

<a name="safe_dict_fetch-2"></a>

### safe_dict_fetch/2 ###

`safe_dict_fetch(Key, Dict) -> any()`

<a name="safe_ets_delete-1"></a>

### safe_ets_delete/1 ###

`safe_ets_delete(TableId) -> any()`

<a name="safe_ets_increment-3"></a>

### safe_ets_increment/3 ###

`safe_ets_increment(TableId, Key, Counters) -> any()`

<a name="safe_ets_lookup_element-2"></a>

### safe_ets_lookup_element/2 ###

`safe_ets_lookup_element(TableId, Key) -> any()`

<a name="tab2list-1"></a>

### tab2list/1 ###

`tab2list(Tid) -> any()`

<a name="unix_tstamp_ms-0"></a>

### unix_tstamp_ms/0 ###

`unix_tstamp_ms() -> any()`

<a name="update_op-1"></a>

### update_op/1 ###

`update_op(Counters) -> any()`

<a name="uuid-0"></a>

### uuid/0 ###

`uuid() -> any()`

