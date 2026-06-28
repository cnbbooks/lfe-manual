# Code Paths

## Path Management

**Code Path Contexts**:

- `deps`: Dependency ebins
- `plugins`: Plugin ebins
- `all_deps`: All dependencies including project apps

**Setting Paths**:

```erlang
rebar_paths:set_paths([deps], State)
rebar_paths:set_paths([deps, plugins], State)
```

## Path Priority

**During Compilation**:

1. Current application's ebin (prepended)
2. All dependencies' ebins
3. All project apps' ebins
4. Rebar3's own dependencies (controlled)

**Erlang Code Path**:

- Managed via `code:add_patha/1` and `code:add_pathz/1`
- `patha`: Front of path (higher priority)
- `pathz`: End of path (lower priority)

## Include Path Resolution

**`-include_lib("app/include/file.hrl")` Resolution**:

1. Find `app` in code path
2. Look for `include/file.hrl` relative to app directory

**Example**:

```erlang
% In code:
-include_lib("cowboy/include/cowboy.hrl").

% Resolves to:
_build/default/lib/cowboy/include/cowboy.hrl
```
