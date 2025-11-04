# Compiling

The single most imporant convenience provided by the LFE rebar3 plugin is
arguably the compiler. This allows any LFE project to be downloaded,
compile, and used by any BEAM language that is also using rebar3 to manage
its dependencies, etc.

To compile an LFE project:

```shell
rebar3 lfe compile
```

If you are publishing your LFE code, or using it in another project, you'll
want to update your `rebar.config` file so that it is compile when a user
(or script) executes the regular `rebar3 compile` command.

To ensure your LFE code will compile in other projects, add the following to
your project's `rebar.config`:

```erlang
{provider_hooks, [
  {pre, [{compile, {lfe, compile}}]}
  ]}.
```
