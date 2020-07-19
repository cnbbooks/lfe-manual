# Cleanup Commands

There are a handful of tasks the author of the plugin as found useful when
debugging LFE applications, rebar3 plugins, etc. These include various
"cleanup" tasks that are not currently supported by `rebar3` (or whose support
is problematic).

Remove the apps' `ebin/*` files:

```shell
rebar3 lfe clean
```

Remove the `_build` directory:

```shell
rebar3 lfe clean-build
```

Remove the the cached directories for the app and the rebar3_lfe plugin, both
global and local:

```shell
rebar3 lfe clean-cache
```

Perform all clean tasks as well as remove the files `erl_crash.dump`, 
`rebar3.crashdump`, and `rebar.lock`:

```shell
rebar3 lfe clean-all
```
