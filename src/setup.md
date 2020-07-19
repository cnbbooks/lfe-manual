# Setup

## Dependencies

In order to use the LFE rebar3 plugin, you need to have the following installed
on your system:

* Erlang (only tested with versions 19 and above)
* `rebar3` (tested with 3.10 and 3.12)

You don't need to download LFE; the plugin will do that for you.

## Using the Plugin

After installing `rebar3`, the only thing you need to do in order to take full
advantage of the LFE rebar3 plugin is add it to the plugins in your global
`rebar.config` file.

> To use the latest stable release, update your `rebar.config` to:

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar2_lfe.git", {tag, "0.2.0"}}}
]}.
```

> If you want to use the current development branch (unstable):

```erlang
{plugins, [
  {rebar3_lfe,
    {git, "https://github.com/lfe-rebar3/rebar2_lfe.git", {branch, "release/0.3.x"}}}
]}.
```
