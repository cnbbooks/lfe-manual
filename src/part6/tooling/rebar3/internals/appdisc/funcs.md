# Functions & API Calls

## `rebar_prv_app_discovery:do/1`

**Purpose**: Main provider entry point for application discovery

**Signature**:

```erlang
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
```

**Arguments**:

- `State` (`rebar_state:t()`): Current state

**Returns**: `{ok, State}` with discovered applications

**Flow**:

1. Get `lib_dirs` from state
2. Call `rebar_app_discover:do/2`
3. Install project app plugins
4. Return updated state

---

## `rebar_app_discover:do/2`

**Purpose**: Main discovery logic

**Signature**:

```erlang
-spec do(rebar_state:t(), [file:filename()]) -> rebar_state:t() | no_return().
```

**Arguments**:

- `State` (`rebar_state:t()`): Current state
- `LibDirs` (`[file:filename()]`): Library directories to scan

**Returns**: Updated state with project apps

**Flow**:

1. Get `src_dirs` from opts
2. Call `find_apps/4` to discover applications
3. Call `define_root_app/2` to determine project type
4. Parse dependencies per profile
5. For each app, call `merge_opts/3` and update state
6. Return state with `project_apps` set

---

## `find_apps/4`

**Purpose**: Find all applications in given directories

**Signature**:

```erlang
-spec find_apps(LibDirs, SrcDirs, Validate, State) -> [rebar_app_info:t()] when
    LibDirs :: [file:filename_all()],
    SrcDirs :: [file:filename_all()],
    Validate :: valid | invalid | all,
    State :: rebar_state:t().
```

**Arguments**:

- `LibDirs`: Directories to search (e.g., `["apps"]`)
- `SrcDirs`: Source directories within each lib dir (e.g., `["src"]`)
- `Validate`: Filter criterion (`all`, `valid`, `invalid`)
- `State`: Current state

**Returns**: List of discovered applications

**Flow**:

1. Call `all_app_dirs/3` to get app directories
2. For each app directory, call `find_app/5`
3. Filter based on validation criterion
4. Return list of app info records

---

## `all_app_dirs/3`

**Purpose**: Find all directories containing applications

**Signature**:

```erlang
-spec all_app_dirs([file:name()], [file:name()], rebar_state:t()) ->
    [{file:name(), [file:name()]}].
```

**Arguments**:

- Library directories
- Source directories
- State

**Returns**: List of `{AppDir, SrcDirs}` tuples

**Flow**:

1. For each lib directory:
   - Build file patterns for app resources
   - Patterns: `lib_dir/src_dir/*.{app,app.src,app.src.script}`
   - Also: `lib_dir/ebin/*.app`, `lib_dir/mix.exs`
2. Use `filelib:wildcard/1` to find matching files
3. Extract app directory from file path
4. Return unique app directories with their src dirs

---

## `find_app/5`

**Purpose**: Discover and validate a single application

**Signature**:

```erlang
-spec find_app(AppInfo, AppDir, SrcDirs, Validate, State) ->
    {true, rebar_app_info:t()} | false when
    AppInfo :: rebar_app_info:t(),
    AppDir :: file:filename_all(),
    SrcDirs :: [file:filename_all()],
    Validate :: valid | invalid | all,
    State :: rebar_state:t().
```

**Arguments**:

- `AppInfo`: Empty or partially-filled app info
- `AppDir`: Application directory
- `SrcDirs`: Source directories
- `Validate`: Validation criterion
- `State`: Current state

**Returns**:

- `{true, AppInfo}` if app found and matches validation
- `false` if app not found or doesn't match validation

**Flow**:

1. Read app's `rebar.config` (if exists)
2. Update app info opts with config
3. Call `find_app_/5` to locate and parse app resource file
4. Return result

---

## `find_app_/5`

**Purpose**: Internal app discovery with resource file handling

**Signature**:

```erlang
-spec find_app_(AppInfo, AppDir, SrcDirs, Validate, State) ->
    {true, rebar_app_info:t()} | false when
    AppInfo :: rebar_app_info:t(),
    AppDir :: file:filename_all(),
    SrcDirs :: [file:filename_all()],
    Validate :: valid | invalid | all,
    State :: rebar_state:t().
```

**Flow**:

1. Get application resource extensions from state (default: `[".app", ".app.src", ".app.src.script"]`)
2. Search for resource files in:
   - `ebin/*.app`
   - `src_dir/*.app.src`
   - `src_dir/*.app.src.script`
   - `mix.exs`
3. Flatten resource files (preferring .app > .script > .app.src)
4. Call `try_handle_resource_files/4` to parse

---

## `try_handle_resource_files/4`

**Purpose**: Parse application resource file and create app info

**Signature**:

```erlang
-spec try_handle_resource_files(AppInfo, AppDir, ResourceFiles, Validate) ->
    {true, rebar_app_info:t()} | false when
    AppInfo :: rebar_app_info:t(),
    AppDir :: file:filename_all(),
    ResourceFiles :: [{app_resource_type(), file:filename()}],
    Validate :: valid | invalid | all.
```

**Flow**:

1. Select first available resource file
2. Based on type:
   - `.app`: Call `rebar_app_info:discover/1` to parse
   - `.app.src`: Call `create_app_info_src/3`
   - `.app.src.script`: Evaluate script, then parse
   - `mix.exs`: Parse Elixir project
3. Validate application based on criterion
4. Return app info or false

---

## `define_root_app/2`

**Purpose**: Determine if project is single-app or umbrella

**Signature**:

```erlang
-spec define_root_app([rebar_app_info:t()], rebar_state:t()) -> root | binary().
```

**Arguments**:

- `Apps`: Discovered applications
- `State`: Current state

**Returns**:

- App name (binary) if single-app project
- `root` atom if umbrella project

**Logic**:

- Check if any app's directory equals project root directory
- If match found: single-app (return app name)
- If no match: umbrella (return `root`)

---

## `parse_profile_deps/5`

**Purpose**: Parse dependencies for a specific profile

**Signature**:

```erlang
-spec parse_profile_deps(Profile, Name, Deps, Opts, State) -> [rebar_app_info:t()] when
    Profile :: atom(),
    Name :: binary() | root,
    Deps :: [term()],
    Opts :: rebar_dict(),
    State :: rebar_state:t().
```

**Arguments**:

- `Profile`: Profile name (default, test, etc.)
- `Name`: Application name or `root`
- `Deps`: Dependency specifications
- `Opts`: Application options
- `State`: Current state

**Returns**: List of parsed dependency app infos

**Flow**:

1. Get dependency directory for profile
2. Get locks from state
3. Call `rebar_app_utils:parse_deps/6`
4. Return list of dependency app info records

---

## `merge_opts/3`

**Purpose**: Merge top-level and app-specific configuration

**Signature**:

```erlang
-spec merge_opts(root | binary(), rebar_app_info:t(), rebar_state:t()) ->
    {rebar_app_info:t(), rebar_state:t()}.
```

**Arguments**:

- `TopLevelApp`: Root app name or `root`
- `AppInfo`: Application to configure
- `State`: Current state

**Returns**: `{UpdatedAppInfo, UpdatedState}`

**Flow**:

1. Reset hooks/plugins if top-level app
2. Apply overrides if not top-level app
3. Apply profiles to app opts
4. Verify OTP version requirements
5. For each profile, handle app dependencies
6. Return updated app info and state
