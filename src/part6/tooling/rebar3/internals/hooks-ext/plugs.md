# Plugins

## Plugin Structure

```
rebar3_my_plugin/
├── src/
│   ├── rebar3_my_plugin.erl        % Plugin entry point
│   ├── rebar3_my_plugin_prv.erl    % Provider implementation
│   └── my_custom_compiler.erl      % Optional custom compiler
├── rebar.config
└── README.md
```

## Plugin Entry Point

```erlang
-module(rebar3_my_plugin).
-export([init/1]).

init(State) ->
    % Register providers
    {ok, State1} = rebar3_my_plugin_prv:init(State),

    % Register custom compilers
    State2 = rebar_state:append_compilers(State1, [my_custom_compiler]),

    {ok, State2}.
```

## Provider Implementation

```erlang
-module(rebar3_my_plugin_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

init(State) ->
    Provider = providers:create([
        {name, my_command},
        {module, ?MODULE},
        {bare, true},
        {deps, [compile]},  % Run after compile
        {example, "rebar3 my_command"},
        {short_desc, "My custom command"},
        {desc, "Longer description"},
        {opts, []}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    % Implement provider logic
    rebar_api:info("Running my_command", []),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
```

## Install Plugin

**Global** (`~/.config/rebar3/rebar.config`):

```erlang
{plugins, [rebar3_my_plugin]}.
```

**Project** (`rebar.config`):

```erlang
{project_plugins, [rebar3_my_plugin]}.
```

**From Source**:

```erlang
{plugins, [
    {rebar3_my_plugin, {git, "https://github.com/user/rebar3_my_plugin.git", {tag, "1.0.0"}}}
]}.
```
