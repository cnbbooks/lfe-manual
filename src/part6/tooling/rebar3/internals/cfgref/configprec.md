# Configuration Precedence

**Order** (later overrides earlier):

1. Built-in defaults
2. `~/.config/rebar3/rebar.config` (global)
3. `rebar.config` (project)
4. Profile-specific configuration
5. Environment variables
6. Command-line options

**Example**:

```erlang
% rebar.config (default):
{erl_opts, [debug_info]}.

% Profile overrides:
{profiles, [{prod, [{erl_opts, [no_debug_info]}]}]}.

% Result with `rebar3 as prod compile`:
{erl_opts, [no_debug_info]}  % prod profile wins
```
