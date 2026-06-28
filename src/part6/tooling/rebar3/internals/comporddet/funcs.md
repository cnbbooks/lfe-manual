# Functions & API Calls

## `rebar_digraph:compile_order/1`

**Purpose**: Sort applications in topological order for compilation

**Signature**:

```erlang
-spec compile_order([rebar_app_info:t()]) ->
    {ok, [rebar_app_info:t()]} | {error, no_sort | {cycles, [[binary()]]}}.
```

**Arguments**:

- `Apps` (`[rebar_app_info:t()]`): Unsorted list of applications

**Returns**:

- `{ok, SortedApps}`: Applications sorted in compilation order
- `{error, {cycles, Cycles}}`: Circular dependencies detected
- `{error, no_sort}`: Sort failed for other reason

**Example Usage**:

```erlang
{ok, Sorted} = rebar_digraph:compile_order(AllApps)
```

**Called From**:

- [Dependency Resolution & Locking](/part6/tooling/rebar3/internals/depres-lock/): `find_cycles/1`
- Stage 05 (this stage): `handle_project_apps/2` in `rebar_prv_compile`

---

## `all_apps_deps/1`

**Purpose**: Get all dependencies for an application

**Signature**:

```erlang
-spec all_apps_deps(rebar_app_info:t()) -> [binary()].
```

**Arguments**:

- `App` (`rebar_app_info:t()`): Application to analyze

**Returns**: List of dependency names (binaries)

**Flow**:

1. Get `applications` list via `rebar_app_info:applications/1`
   - Returns list of atoms from `.app` file
   - These are runtime OTP dependencies
2. Get `deps` list via `rebar_app_info:deps/1`
   - Returns list from `rebar.config`
   - Format: `[{Name, Spec}]` or `[Name]`
   - These are build-time rebar3 dependencies
3. Convert all to binaries
4. Sort each list
5. Merge with `lists:umerge/2` (union merge)
6. Return combined unique list

**Example**:

```erlang
% my_app.app:
{applications, [kernel, stdlib, cowboy]}

% rebar.config:
{deps, [jsx, {ranch, "1.8.0"}]}

% Result:
[<<"cowboy">>, <<"jsx">>, <<"kernel">>, <<"ranch">>, <<"stdlib">>]
```

---

## `add/2` (internal)

**Purpose**: Add application and its dependencies to digraph

**Signature**:

```erlang
-spec add(digraph:graph(), {PkgName, [Dep]}) -> ok when
    PkgName :: binary(),
    Dep :: {Name, term()} | Name,
    Name :: atom() | iodata().
```

**Arguments**:

- `Graph`: Existing digraph
- `{PkgName, Deps}`: Application name and dependency list

**Returns**: `ok`

**Flow**:

1. Check if vertex exists for `PkgName`
2. If not: add vertex with `digraph:add_vertex/2`
3. For each dependency:
   - Extract dependency name (handle tuple format)
   - Convert to binary
   - Add vertex for dependency if not exists
   - Add edge: `PkgName → DependencyName`

**Edge Direction**: Application points TO its dependency

---

## `names_to_apps/2` (internal)

**Purpose**: Map sorted names back to app info records

**Signature**:

```erlang
-spec names_to_apps([atom()], [rebar_app_info:t()]) -> [rebar_app_info:t()].
```

**Arguments**:

- `Names`: Sorted list of application names
- `Apps`: Original unsorted app info records

**Returns**: Sorted app info records

**Flow**:

1. For each name in sorted list:
   - Find corresponding app info via `find_app_by_name/2`
   - If found, include in result
   - If not found, skip
2. Return list preserving sort order

---

## `find_app_by_name/2` (internal)

**Purpose**: Find app info by name

**Signature**:

```erlang
-spec find_app_by_name(atom(), [rebar_app_info:t()]) ->
    {ok, rebar_app_info:t()} | error.
```

**Arguments**:

- `Name`: Application name to find
- `Apps`: List of app info records

**Returns**:

- `{ok, AppInfo}`: App found
- `error`: App not found

---

## `rebar_prv_install_deps:find_cycles/1`

**Purpose**: Wrapper for cycle detection in dependency resolution

**Signature**:

```erlang
-spec find_cycles([rebar_app_info:t()]) ->
    {no_cycle, Sorted} | {cycles, Cycles} | {error, Error} when
    Sorted :: [rebar_app_info:t()],
    Cycles :: [[binary()]],
    Error :: term().
```

**Arguments**:

- `Apps`: Applications to check

**Returns**:

- `{no_cycle, Sorted}`: No cycles, returns sorted list
- `{cycles, Cycles}`: Circular dependencies found
- `{error, Error}`: Other error

**Flow**:

1. Call `rebar_digraph:compile_order/1`
2. Transform result format
3. Return appropriate tuple

---

## `rebar_prv_install_deps:cull_compile/2`

**Purpose**: Filter sorted dependencies to those needing compilation

**Signature**:

```erlang
-spec cull_compile([rebar_app_info:t()], [rebar_app_info:t()]) ->
    [rebar_app_info:t()].
```

**Arguments**:

- `TopSortedDeps`: All apps in compilation order
- `ProjectApps`: Project's own applications

**Returns**: Filtered list of dependencies to compile

**Flow**:

1. Remove project apps from sorted list (using `--` operator)
2. Drop dependencies from beginning until finding one that needs compilation
3. Use `lists:dropwhile/2` with `not_needs_compile/1` predicate
4. Return remaining list

**Logic**: Once we hit a dep that needs compile, compile all remaining deps

---

## `not_needs_compile/1` (internal)

**Purpose**: Determine if dependency doesn't need compilation

**Criteria**:

- Package from Hex with pre-built artifacts: doesn't need compile
- Source dependency (Git, Hg): needs compile
- Checkout dependency: needs compile

**Implementation**: Checks `rebar_app_info` fields and source type
