

# Module gitignore #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accepts-2">accepts/2</a></td><td>
Return true if the given <tt>File</tt> is accepted, false otherwise.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>
Terminate the gitignore gen_server for the  given <tt>Pid</tt></td></tr><tr><td valign="top"><a href="#compile-1">compile/1</a></td><td>
Compile the given list of gitignore patterns.</td></tr><tr><td valign="top"><a href="#denies-2">denies/2</a></td><td>
Return true if the given <tt>File</tt> is denied, false otherwise.</td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td>
Open en compile the given gitignore file.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accepts-2"></a>

### accepts/2 ###

<pre><code>
accepts(Pid::pid(), File::string()) -&gt; true | false
</code></pre>
<br />

Return true if the given `File` is accepted, false otherwise

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Pid::pid()) -&gt; ok
</code></pre>
<br />

Terminate the gitignore gen_server for the  given `Pid`

<a name="compile-1"></a>

### compile/1 ###

<pre><code>
compile(Data::list()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

Compile the given list of gitignore patterns

<a name="denies-2"></a>

### denies/2 ###

<pre><code>
denies(Pid::pid(), File::string()) -&gt; true | false
</code></pre>
<br />

Return true if the given `File` is denied, false otherwise

<a name="open-1"></a>

### open/1 ###

<pre><code>
open(File::iodata()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

Open en compile the given gitignore file

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(FileOrData::list() | iodata(), Type::file | data) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

