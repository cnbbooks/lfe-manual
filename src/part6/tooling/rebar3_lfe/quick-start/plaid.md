# Going Plaid

<img src="https://raw.githubusercontent.com/lfe/docs/master/docs/current/images/plaid.jpg"
     style="float: right;">


Remember: this _is_ a quick-start; it's going to be a blur of colour! You're not going to get very many
details here, but you _will_ get to jump into the LFE REPL and see a little
code in action.<sup>1</sup>

The rest of this quick-start assumes that you've followed the links in the previous section and have installed both Erlang as well as `rebar3`, but to take things further, you'll need to do one more thing: set up the LFE plugin.

Each project you create with the LFE rebar3 plugin will generate a `rebar.config` file that automatically includes the plugin dependency, but that's only inside an LFE project. You need to bootstrap the LFE plugin by setting it up in your global `rebar.config`.

The [rebar3 docs](https://www.rebar3.org/docs/using-available-plugins) tell you this file is located at `~/.config/rebar3/rebar.config`. To set this up, you can safely execute the following in a terminal, even if the file already exists:

```shell
mkdir -p ~/.config/rebar3/
touch ~/.config/rebar3/rebar.config
```

Then, in your preferred editor, open that file and add the entry for LFE rebar3 plugin. If that file is empty when you open it, then you can paste this whole thing in there:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar3_lfe.git", {branch, "main"}}}
]}.
```

If you want to pin your project to a specific release of the plugin, you can view the list of released versions here:
* https://github.com/lfe-rebar3/rebar3_lfe/tags

And then use `tag` (with the version) instead of `branch`:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar3_lfe.git", {tag, "x.y.z"}}}
]}.
```

If your global rebar3 config file already has one or more plugins in it, then simply add a comma after the last one and paste the `{rebar3_lfe ...}` line from above (with no trailing comma!).


## Next Stop

Ready for some LFE? Next you'll learn how to create a new LFE project with
just one command ...




<div class="footnotes">
<hr />
<ol>
<li>For those that would enjoy a more in-depth introduction and would appreciate having the time to see the stars (and not just stunning plaid), you may be
interested in checking out <a href="http://lfe.io/tutorial/">The LFE Tutorial</a>.
</li>
</ol>
</div>
