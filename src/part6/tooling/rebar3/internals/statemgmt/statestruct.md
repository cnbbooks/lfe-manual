# `rebar_state` Structure

## Record Definition

```erlang
-record(state_t, {
    dir                               :: file:name(),
    opts                = dict:new()  :: rebar_dict(),
    code_paths          = dict:new()  :: rebar_dict(),
    default             = dict:new()  :: rebar_dict(),
    escript_path                      :: undefined | file:filename_all(),
    lock                = [],
    current_profiles    = [default]   :: [atom()],
    namespace           = default     :: atom(),
    command_args        = [],
    command_parsed_args = {[], []},
    current_app                       :: undefined | rebar_app_info:t(),
    project_apps        = []          :: [rebar_app_info:t()],
    deps_to_build       = []          :: [rebar_app_info:t()],
    all_plugin_deps     = []          :: [rebar_app_info:t()],
    all_deps            = []          :: [rebar_app_info:t()],
    compilers           = []          :: [module()],
    project_builders    = []          :: [{project_type(), module()}],
    resources           = [],
    providers           = [],
    allow_provider_overrides = false  :: boolean()
}).
```

## Key Fields

| Field | Type | Purpose |
|-------|------|---------|
| `dir` | `file:name()` | Project root directory |
| `opts` | `rebar_dict()` | Configuration from `rebar.config` |
| `default` | `rebar_dict()` | Original opts before profile application |
| `current_profiles` | `[atom()]` | Active profiles (e.g., `[default, test]`) |
| `project_apps` | `[rebar_app_info:t()]` | Project applications |
| `deps_to_build` | `[rebar_app_info:t()]` | Dependencies needing compilation |
| `all_deps` | `[rebar_app_info:t()]` | All resolved dependencies |
| `compilers` | `[module()]` | Registered compiler modules |
| `providers` | `[providers:t()]` | Available commands/providers |
| `lock` | `[lock_entry()]` | Lock file data |

## Common Operations

**Create New State**:

```erlang
State = rebar_state:new(Config)
```

**Get/Set Configuration**:

```erlang
Value = rebar_state:get(State, Key, Default),
State1 = rebar_state:set(State, Key, Value)
```

**Manage Applications**:

```erlang
Apps = rebar_state:project_apps(State),
State1 = rebar_state:project_apps(State, UpdatedApps),

Deps = rebar_state:all_deps(State),
State2 = rebar_state:update_all_deps(State1, UpdatedDeps)
```

**Apply Profiles**:

```erlang
State1 = rebar_state:apply_profiles(State, [test])
```

**Manage Compilers**:

```erlang
State1 = rebar_state:prepend_compilers(State, [my_compiler]),
State2 = rebar_state:append_compilers(State1, [another_compiler]),
Compilers = rebar_state:compilers(State2)
```
