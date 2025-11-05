# Hook Configuration

## pre_hooks / post_hooks

**Purpose**: Execute shell commands before/after providers

**Type**: `[{Provider, Command}]` or `[{Arch, Provider, Command}]`

**Examples**:

```erlang
{pre_hooks, [
    {compile, "make -C c_src"},
    {clean, "make -C c_src clean"}
]}.

{post_hooks, [
    {compile, "./scripts/post_build.sh"}
]}.

% Platform-specific
{pre_hooks, [
    {linux, compile, "./configure"},
    {darwin, compile, "./configure.macos"},
    {win32, compile, "configure.bat"}
]}.
```

## provider_hooks

**Purpose**: Run providers before/after other providers

**Type**: `[{pre | post, [{Provider, HookProvider}]}]`

**Example**:

```erlang
{provider_hooks, [
    {pre, [
        {compile, {pc, compile}},
        {clean, {pc, clean}}
    ]},
    {post, [
        {compile, {my_plugin, post_compile}}
    ]}
]}.
```
