# `rebar_app_info` Structure

## Record Definition

```erlang
-record(app_info_t, {
    name               :: binary() | undefined,
    app_file_src       :: file:filename_all() | undefined,
    app_file_src_script:: file:filename_all() | undefined,
    app_file           :: file:filename_all() | undefined,
    original_vsn       :: app_vsn() | undefined,
    vsn                :: app_vsn() | undefined,
    parent = root      :: binary() | root,
    app_details = []   :: list(),
    applications = []  :: list(),
    included_applications = [] :: [atom()],
    optional_applications = [] :: [atom()],
    deps = []          :: list(),
    profiles = [default] :: [atom()],
    default = dict:new() :: rebar_dict(),
    opts = dict:new()  :: rebar_dict(),
    dep_level = 0      :: integer(),
    fetch_dir          :: file:name(),
    dir                :: file:name(),
    out_dir            :: file:name(),
    ebin_dir           :: file:name(),
    source             :: source_spec(),
    is_lock = false    :: boolean(),
    is_checkout = false :: boolean(),
    valid              :: boolean() | undefined,
    project_type       :: rebar3 | mix | undefined,
    is_available = false :: boolean()
}).
```

## Key Fields

| Field | Type | Purpose |
|-------|------|---------|
| `name` | `binary()` | Application name |
| `dir` | `file:name()` | Source directory |
| `out_dir` | `file:name()` | Build output directory |
| `ebin_dir` | `file:name()` | Compiled `.beam` files location |
| `app_file_src` | `file:filename()` | Path to `.app.src` |
| `app_file` | `file:filename()` | Path to `.app` |
| `vsn` | `string()` | Application version |
| `applications` | `[atom()]` | Runtime dependencies |
| `deps` | `[term()]` | Build dependencies |
| `opts` | `rebar_dict()` | App-specific configuration |
| `dep_level` | `integer()` | Depth in dependency tree |
| `is_checkout` | `boolean()` | Whether from `_checkouts/` |
| `project_type` | `atom()` | `rebar3`, `mix`, etc. |

## Common Operations

**Create New AppInfo**:

```erlang
AppInfo = rebar_app_info:new(AppName, Vsn, Dir)
```

**Get/Set Fields**:

```erlang
Name = rebar_app_info:name(AppInfo),
AppInfo1 = rebar_app_info:name(AppInfo, NewName),

Dir = rebar_app_info:dir(AppInfo),
AppInfo2 = rebar_app_info:dir(AppInfo1, NewDir)
```

**Manage Configuration**:

```erlang
Opts = rebar_app_info:opts(AppInfo),
AppInfo1 = rebar_app_info:opts(AppInfo, NewOpts),

Value = rebar_app_info:get(AppInfo, Key, Default),
AppInfo2 = rebar_app_info:set(AppInfo1, Key, Value)
```

**Apply Profiles**:

```erlang
AppInfo1 = rebar_app_info:apply_profiles(AppInfo, [test, prod])
```
