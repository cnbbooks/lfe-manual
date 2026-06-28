# Configuration

## Hook Configuration

```erlang
% Shell hooks
{pre_hooks, [
    {compile, "make -C c_src"}
]}.

{post_hooks, [
    {clean, "make -C c_src clean"}
]}.

% Provider hooks
{provider_hooks, [
    {pre, [{compile, {pc, compile}}]},
    {post, [{clean, {pc, clean}}]}
]}.

% Platform-specific
{pre_hooks, [
    {linux, compile, "echo 'Linux build'"},
    {darwin, compile, "echo 'macOS build'"}
]}.
```

## Custom Compiler Registration

```erlang
% In plugin init/1:
State1 = rebar_state:prepend_compilers(State, [my_gen_compiler]),
State2 = rebar_state:append_compilers(State1, [my_post_compiler]).
```

## Project Builders

For non-rebar3 projects (e.g., Mix, Make):

```erlang
% Register builder
State1 = rebar_state:add_project_builder(State, mix, rebar3_mix_builder).
```

**Builder Module**:

```erlang
-module(rebar3_mix_builder).
-export([build/1]).

build(AppInfo) ->
    % Build using Mix
    rebar_utils:sh("mix compile", [{cd, rebar_app_info:dir(AppInfo)}]),
    ok.
```
