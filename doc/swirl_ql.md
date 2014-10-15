

# Module swirl_ql #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-boolean_op">boolean_op()</a> ###



<pre><code>
boolean_op() = 'and' | 'or'
</code></pre>





### <a name="type-comparison_op">comparison_op()</a> ###



<pre><code>
comparison_op() = '&lt;' | '&lt;=' | '=' | '&gt;=' | '&gt;' | '&lt;&gt;'
</code></pre>





### <a name="type-event">event()</a> ###



<pre><code>
event() = #{}
</code></pre>





### <a name="type-exp_tree">exp_tree()</a> ###



<pre><code>
exp_tree() = {<a href="#type-boolean_op">boolean_op()</a>, <a href="#type-exp_tree">exp_tree()</a>, <a href="#type-exp_tree">exp_tree()</a>} | {<a href="#type-comparison_op">comparison_op()</a>, atom(), <a href="#type-value">value()</a>} | {<a href="#type-inclusion_op">inclusion_op()</a>, atom(), [<a href="#type-value">value()</a>, ...]} | {<a href="#type-null_op">null_op()</a>, atom()}
</code></pre>





### <a name="type-inclusion_op">inclusion_op()</a> ###



<pre><code>
inclusion_op() = in | notin
</code></pre>





### <a name="type-null_op">null_op()</a> ###



<pre><code>
null_op() = null | notnull
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = integer() | float() | binary()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#evaluate-2">evaluate/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="evaluate-2"></a>

### evaluate/2 ###


<pre><code>
evaluate(X1::<a href="#type-exp_tree">exp_tree()</a>, Vars::<a href="#type-event">event()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="parse-1"></a>

### parse/1 ###


<pre><code>
parse(String::string() | binary()) -&gt; {ok, <a href="#type-exp_tree">exp_tree()</a>} | {error, term()}
</code></pre>
<br />


