# Functions & API Calls

## `rebar_otp_app:compile/2`

**Purpose**: Main entry point for .app file generation

**Signature**:

```erlang
-spec compile(State, App) -> {ok, UpdatedApp} | {error, Reason} when
    State :: rebar_state:t(),
    App :: rebar_app_info:t(),
    UpdatedApp :: rebar_app_info:t(),
    Reason :: term().
```

**Flow**:

1. Check for `.app.src.script` file
2. If not found, check for `.app.src` file
3. If found, call `preprocess/3`
4. Call `validate_app/2` to verify result
5. Return updated app info

---

## `preprocess/3`

**Purpose**: Process .app.src into .app file

**Signature**:

```erlang
-spec preprocess(State, AppInfo, AppSrcFile) -> UpdatedAppInfo when
    State :: rebar_state:t(),
    AppInfo :: rebar_app_info:t(),
    AppSrcFile :: file:filename(),
    UpdatedAppInfo :: rebar_app_info:t().
```

**Arguments**:

- `State`: Rebar state
- `AppInfo`: Application information record
- `AppSrcFile`: Path to `.app.src` or `.app.src.script`

**Returns**: Updated app info with `.app` file path and version

**Flow**:

1. Read and parse `AppSrcFile`
2. Load app vars via `load_app_vars/1`
3. Generate module list via `ebin_modules/2`
4. Apply vars via `apply_app_vars/2`
5. Determine version via `app_vsn/4`
6. Ensure `registered` field exists
7. Ensure `description` field exists
8. Format as Erlang term
9. Write to `.app` file
10. Return updated app info

---

## `load_app_vars/1`

**Purpose**: Load variables for substitution

**Signature**:

```erlang
-spec load_app_vars(State :: rebar_state:t()) -> [{Key, Value}] when
    Key :: atom(),
    Value :: term().
```

**Configuration**: `app_vars_file` in `rebar.config`

**Example**:

```erlang
% rebar.config:
{app_vars_file, "config/app.vars"}.

% config/app.vars:
{copyright, "Copyright (c) 2024 My Company"}.
{author, "John Doe"}.
```

**Returns**: List of `{Key, Value}` tuples

---

## `ebin_modules/2`

**Purpose**: Generate list of compiled modules

**Signature**:

```erlang
-spec ebin_modules(AppInfo, Dir) -> [Module] when
    AppInfo :: rebar_app_info:t(),
    Dir :: file:filename(),
    Module :: atom().
```

**Flow**:

1. Find all `.beam` files in `Dir/ebin/`
2. Get `extra_src_dirs` configuration
3. Filter out modules from extra directories
4. Convert beam file names to module atoms
5. Return sorted module list

**Why Filter Extra Dirs**: Modules in `extra_src_dirs` shouldn't be in the main `.app` file's modules list (they're for tests, scripts, etc.)

---

## `extra_dirs/1`

**Purpose**: Get extra source directories (excluding normal src_dirs)

**Returns**: List of directories like `["test", "scripts"]`

---

## `in_extra_dir/3`

**Purpose**: Check if beam file originated from extra directory

**Uses**: `beam_lib:chunks/2` to read `compile_info` and find original source path

---

## `apply_app_vars/2`

**Purpose**: Substitute variables in application data

**Signature**:

```erlang
-spec apply_app_vars(Vars, AppData) -> UpdatedAppData when
    Vars :: [{Key, Value}],
    AppData :: [tuple()],
    UpdatedAppData :: [tuple()].
```

**Logic**: For each `{Key, Value}`, replace `{Key, _}` in `AppData` with `{Key, Value}`

**Example**:

```erlang
AppVars = [{modules, [mod1, mod2]}, {custom_key, "value"}]
AppData = [{vsn, "1.0.0"}, {modules, []}, {custom_key, undefined}]

Result = [{vsn, "1.0.0"}, {modules, [mod1, mod2]}, {custom_key, "value"}]
```

---

## `app_vsn/4`

**Purpose**: Determine application version

**Signature**:

```erlang
-spec app_vsn(AppInfo, AppData, AppFile, State) -> Version when
    AppInfo :: rebar_app_info:t(),
    AppData :: [tuple()],
    AppFile :: file:filename(),
    State :: rebar_state:t(),
    Version :: string().
```

**Calls**: `rebar_utils:vcs_vsn/3`

**Version Specifications**:

1. **Literal String**:

   ```erlang
   {vsn, "1.2.3"}
   ```

   Returns: `"1.2.3"`

2. **Git Tag**:

   ```erlang
   {vsn, git}
   ```

   Returns: Latest git tag, e.g., `"v1.2.3"` or `"1.2.3-15-gabc123"` if commits after tag

3. **Git Short/Long**:

   ```erlang
   {vsn, {git, short}}  % 7-char SHA
   {vsn, {git, long}}   % 40-char SHA
   ```

4. **Semver from Git**:

   ```erlang
   {vsn, semver}
   ```

   Calculates semantic version from git history

5. **Command**:

   ```erlang
   {vsn, {cmd, "git describe --tags"}}
   ```

   Executes command, uses output

---

## `ensure_registered/1`

**Purpose**: Ensure `registered` field exists

**Signature**:

```erlang
-spec ensure_registered(AppData) -> UpdatedAppData when
    AppData :: [tuple()],
    UpdatedAppData :: [tuple()].
```

**Logic**:

- If `registered` key exists: keep it
- If not: add `{registered, []}`

**Reason**: Required by `systools:make_relup/4`

---

## `ensure_description/1`

**Purpose**: Ensure `description` field exists

**Similar to `ensure_registered/1`**

**Adds**: `{description, ""}` if missing

**Reason**: Required for releases

---

## `write_file_if_contents_differ/3`

**Purpose**: Write file only if contents changed

**Benefit**: Preserves timestamp if content unchanged, preventing unnecessary recompilation

---

## `validate_app/2`

**Purpose**: Verify generated .app file is valid

**Flow**:

1. Read `.app` file
2. Parse as Erlang term
3. Verify format: `{application, AppName, AppData}`
4. Validate name matches filename
5. Optionally validate modules exist

---

## `validate_name/2`

**Purpose**: Ensure app name matches filename

**Example**:

- File: `ebin/my_app.app`
- Expected name: `my_app`
- Actual name in file: must be `my_app`

**Error if mismatch**: Prevents deployment issues

---

## `validate_app_modules/3`

**Purpose**: Verify all listed modules exist as `.beam` files

**Configuration**: `validate_app_modules` (default: `true`)

**Checks**:

- Every module in `modules` list has corresponding `.beam` file
- No missing modules
- No extra unlisted modules (warning only)
