# Functions & API Calls

## `rebar_prv_install_deps:do/1`

**Purpose**: Main entry point for dependency resolution

**Signature**:

```erlang
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
```

**Arguments**:

- `State` (`rebar_state:t()`): Current state with configuration

**Returns**:

- `{ok, State}`: Dependencies resolved and state updated
- `{error, Reason}`: Resolution failed

**Flow**:

1. Get current profiles
2. Get project apps
3. Check if upgrade mode is enabled
4. Resolve deps per profile via `deps_per_profile/3`
5. Update state with all deps
6. Build and update code paths
7. Detect cycles via `find_cycles/1`
8. Determine compile order via `cull_compile/2`
9. Update state with `deps_to_build`

**Called From**: Provider system (as dependency of `lock` and `compile`)

---

## `deps_per_profile/3`

**Purpose**: Collect and resolve dependencies for all active profiles

**Signature**:

```erlang
-spec deps_per_profile([atom()], boolean(), rebar_state:t()) ->
    {[rebar_app_info:t()], rebar_state:t()}.
```

**Arguments**:

- `Profiles` (`[atom()]`): Active profiles (e.g., `[default, test]`)
- `Upgrade` (`boolean()`): Whether to upgrade dependencies
- `State` (`rebar_state:t()`): Current state

**Returns**:

- `{Apps, State}`: Resolved dependencies and updated state

**Flow**:

1. Get locks from state
2. For each profile, get parsed dependencies at level 0
3. Create `RootSeen` set with project app names
4. Call `handle_profile_level/7` for traversal

---

## `handle_profile_level/7`

**Purpose**: Level-order traversal of dependency tree across all profiles

**Signature**:

```erlang
-spec handle_profile_level(
    [{Profile, Deps, Level}],
    Apps,
    RootSeen,
    Seen,
    Upgrade,
    Locks,
    State
) -> {Apps, State} when
    Profile :: atom(),
    Deps :: [rebar_app_info:t()],
    Level :: integer(),
    Apps :: [rebar_app_info:t()],
    RootSeen :: sets:set(),
    Seen :: sets:set(),
    Upgrade :: boolean(),
    Locks :: [term()],
    State :: rebar_state:t().
```

**Arguments**:

- Profile/Deps/Level tuples: Dependencies per profile at each level
- `Apps`: Accumulated resolved dependencies
- `RootSeen`: Set of top-level app names (never process as deps)
- `Seen`: Set of already-processed dependency names
- `Upgrade`: Whether upgrading
- `Locks`: Lock file data
- `State`: Current state

**Returns**: `{Apps, State}` with all resolved dependencies

**Algorithm**:

```
For each {Profile, Deps, Level}:
  For each Dep in Deps:
    If Dep is in RootSeen:
      Skip (it's a top-level app)
    Else if Dep is in Seen:
      Check for version conflicts, warn if needed
    Else:
      Lock the dependency
      Fetch the dependency
      Parse the dep's own dependencies
      Add new deps to next level

  If new deps were found:
    Append {Profile, NewDeps, Level+1} to queue

  Process next level
```

---

## `update_dep/9`

**Purpose**: Process a single dependency

**Signature**:

```erlang
-spec update_dep(
    AppInfo,
    Profile,
    Level,
    Deps,
    Apps,
    State,
    Upgrade,
    Seen,
    Locks
) -> {NewDeps, NewApps, NewState, NewSeen} when
    AppInfo :: rebar_app_info:t(),
    Profile :: atom(),
    Level :: integer(),
    Deps :: [rebar_app_info:t()],
    Apps :: [rebar_app_info:t()],
    State :: rebar_state:t(),
    Upgrade :: boolean(),
    Seen :: sets:set(),
    Locks :: [term()].
```

**Arguments**:

- `AppInfo`: Dependency to process
- `Profile`: Current profile
- `Level`: Current level in dependency tree
- `Deps`: Accumulated dependencies for next level
- `Apps`: All resolved apps so far
- `State`: Current state
- `Upgrade`: Upgrade flag
- `Seen`: Set of seen dependency names
- `Locks`: Lock data

**Returns**: Updated accumulator tuple

**Flow**:

1. Get dependency name
2. Check if already seen
3. If seen: check for conflicts, possibly warn
4. If not seen:
   - Lock the dependency
   - Fetch/verify the dependency
   - Handle the dependency (parse its deps)
   - Add to accumulated apps
   - Add transitive deps to next level

---

## `maybe_lock/5`

**Purpose**: Add dependency to lock list if appropriate

**Signature**:

```erlang
-spec maybe_lock(Profile, AppInfo, Seen, State, Level) -> {NewSeen, NewState} when
    Profile :: atom(),
    AppInfo :: rebar_app_info:t(),
    Seen :: sets:set(),
    State :: rebar_state:t(),
    Level :: integer().
```

**Arguments**:

- `Profile`: Current profile
- `AppInfo`: Dependency to potentially lock
- `Seen`: Set of seen dependencies
- `State`: Current state
- `Level`: Depth in dependency tree

**Returns**: `{NewSeen, NewState}` with updated lock

**Logic**:

- Skip if checkout dependency
- Skip if not in default profile
- If already in lock at deeper level, replace with shallower
- Otherwise add to lock list
- Always add to seen set

---

## `find_cycles/1`

**Purpose**: Detect circular dependencies

**Signature**:

```erlang
-spec find_cycles([rebar_app_info:t()]) ->
    {no_cycle, Sorted} | {cycles, Cycles} | {error, Error} when
    Sorted :: [rebar_app_info:t()],
    Cycles :: [[binary()]],
    Error :: term().
```

**Arguments**:

- `Apps` (`[rebar_app_info:t()]`): All applications (project + deps)

**Returns**:

- `{no_cycle, Sorted}`: No cycles; sorted topologically
- `{cycles, Cycles}`: Circular dependencies detected
- `{error, Error}`: Other error

**Flow**:

1. Call `rebar_digraph:compile_order/1`
2. Which creates digraph and calls `digraph_utils:topsort/1`
3. If sort succeeds: return sorted list
4. If sort fails: find strongly connected components
5. Filter components with length > 1 (these are cycles)
6. Return cycles

---

## `rebar_digraph:compile_order/1`

**Purpose**: Build dependency graph and return topological sort

**Signature**:

```erlang
-spec compile_order([rebar_app_info:t()]) ->
    {ok, [rebar_app_info:t()]} | {error, no_sort | {cycles, [[binary()]]}}.
```

**Arguments**:

- `Apps` (`[rebar_app_info:t()]`): Applications to sort

**Returns**:

- `{ok, Sorted}`: Topologically sorted applications
- `{error, {cycles, Cycles}}`: Circular dependencies found
- `{error, no_sort}`: Topological sort failed for other reason

**Flow**:

1. Create new digraph
2. For each app:
   - Add vertex with app name
   - Get all dependencies (from `applications` list and `deps` config)
   - Add edges from app to each dependency
3. Call `digraph_utils:topsort/1`
4. If successful: reverse the list (dependencies first)
5. If failed: determine if cyclic, extract cycles
6. Delete digraph
7. Return result

**Example Graph**:

```
my_app → [cowboy, jsx]
cowboy → [cowlib, ranch]
cowlib → []
ranch → []
jsx → []

Sorted: [cowlib, ranch, jsx, cowboy, my_app]
```

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

1. Get `applications` list from `.app` file (runtime deps)
2. Get `deps` list from `rebar.config` (build deps)
3. Convert all to binaries
4. Sort and merge (union)

**Why Both Sources**:

- `applications`: Runtime dependencies declared in `.app`
- `deps`: Build-time dependencies from `rebar.config`
- Union ensures all dependencies are considered for build order

---

## `cull_compile/2`

**Purpose**: Filter dependency list to those needing compilation

**Signature**:

```erlang
-spec cull_compile([rebar_app_info:t()], [rebar_app_info:t()]) -> [rebar_app_info:t()].
```

**Arguments**:

- `TopSortedDeps` (`[rebar_app_info:t()]`): All apps in compile order
- `ProjectApps` (`[rebar_app_info:t()]`): Project's own applications

**Returns**: Dependencies that need compilation

**Flow**:

1. Remove project apps from sorted list (they're compiled separately)
2. Drop dependencies from start of list until finding one that needs compile
3. Return remaining list

**Logic for "needs compile"**:

- Checkout dependencies always need compile
- Package dependencies from Hex usually don't (pre-compiled)
- Source dependencies (Git, etc.) need compile

---

## `rebar_prv_lock:do/1`

**Purpose**: Write dependency locks to `rebar.lock`

**Signature**:

```erlang
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
```

**Arguments**:

- `State` (`rebar_state:t()`): Current state with resolved deps

**Returns**: `{ok, State}` with locks saved

**Flow**:

1. Check if running in default profile (only lock in default)
2. Get old locks from state
3. Build new locks via `build_locks/1`
4. Sort locks alphabetically
5. Write to `rebar.lock` if changed
6. Update state with new locks
7. Report useless locks (removed dependencies)
8. Report checkout dependencies (can't be locked)

---

## `build_locks/1`

**Purpose**: Convert state's lock data to lock file format

**Signature**:

```erlang
-spec build_locks(rebar_state:t()) -> [lock_entry()].
```

**Arguments**:

- `State` (`rebar_state:t()`): Current state

**Returns**: List of lock entries

**Lock Entry Format**:

```erlang
{Name :: binary(),
 Source :: lock_source(),
 Level :: integer()}
```

**Example**:

```erlang
{<<"cowboy">>,
 {git, "https://github.com/ninenines/cowboy.git",
      {ref, "abc123def456..."}},
 0}
```

**Flow**:

1. Get all locked deps from state
2. Filter out checkout dependencies
3. For each dep:
   - Get name
   - Get lock source via `rebar_fetch:lock_source/2`
   - Get dependency level
   - Create tuple
