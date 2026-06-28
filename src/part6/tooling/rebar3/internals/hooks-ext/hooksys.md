# Hook System

## Hook Types

**1. Shell Hooks**: Execute shell commands

**Configuration**:

```erlang
{pre_hooks, [
    {compile, "echo 'Starting compilation'"},
    {compile, "./scripts/pre_compile.sh"}
]}.

{post_hooks, [
    {compile, "echo 'Compilation complete'"}
]}.
```

**2. Provider Hooks**: Run other rebar3 providers

**Configuration**:

```erlang
{provider_hooks, [
    {pre, [
        {compile, {my_namespace, my_provider}}
    ]},
    {post, [
        {compile, clean}
    ]}
]}.
```

**3. Platform-Specific Hooks**:

```erlang
{pre_hooks, [
    {linux, compile, "./configure"},
    {darwin, compile, "./configure.macos"},
    {win32, compile, "configure.bat"}
]}.
```

## Hook Execution Points

Within compilation ([Source File Compilation](/part6/tooling/rebar3/internals/srcflcomp)):

- **compile**: Around entire compilation
- **erlc_compile**: Around source file compilation
- **app_compile**: Around `.app` file generation

**Example Flow**:

```
pre compile hooks
  pre erlc_compile hooks
    [Source compilation]
  post erlc_compile hooks
  pre app_compile hooks
    [.app file generation]
  post app_compile hooks
post compile hooks
```

## API: run_all_hooks/5,6

**Signature**:

```erlang
-spec run_all_hooks(Dir, Type, Command, Providers, AppInfo, State) -> AppInfo when
    Dir :: file:filename(),
    Type :: pre | post,
    Command :: atom(),
    Providers :: [providers:t()],
    AppInfo :: rebar_app_info:t(),
    State :: rebar_state:t().
```

**Flow**:

1. Run provider hooks for this command
2. Run shell hooks for this command
3. Return updated app info
