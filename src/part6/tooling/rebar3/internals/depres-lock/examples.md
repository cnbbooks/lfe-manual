# Example Scenarios

## Scenario 1: Simple Project with Hex Dependencies

**`rebar.config`**:

```erlang
{deps, [
    jsx,
    {cowboy, "2.9.0"}
]}.
```

**Execution**: `rebar3 compile` (first time, no lock file)

**Flow**:

1. Parse deps: `jsx` (latest), `cowboy` "2.9.0"
2. Query Hex registry for `jsx` latest → "3.1.0"
3. Resolve `cowboy` → "2.9.0"
4. Get cowboy's dependencies → `[cowlib, ranch]`
5. Resolve cowlib and ranch versions from cowboy's requirements
6. Build dependency graph:

   ```
   my_app → [jsx, cowboy]
   cowboy → [cowlib, ranch]
   jsx → []
   cowlib → []
   ranch → []
   ```

7. Topological sort → `[cowlib, ranch, jsx, cowboy, my_app]`
8. Write lock file with all versions
9. Return deps to build: `[jsx, cowlib, ranch, cowboy]`

**`rebar.lock` created**:

```erlang
{"1.2.0",
[{<<"cowboy">>, {pkg, <<"cowboy">>, <<"2.9.0">>}, 0},
 {<<"cowlib">>, {pkg, <<"cowlib">>, <<"2.11.0">>}, 1},
 {<<"jsx">>, {pkg, <<"jsx">>, <<"3.1.0">>}, 0},
 {<<"ranch">>, {pkg, <<"ranch">>, <<"1.8.0">>}, 1}]}.
[{pkg_hash, [...]}].
```

---

## Scenario 2: Circular Dependency Detection

**Setup**:

```
app_a/rebar.config: {deps, [app_b]}.
app_b/rebar.config: {deps, [app_c]}.
app_c/rebar.config: {deps, [app_a]}.
```

**Execution**: `rebar3 compile`

**Flow**:

1. Resolve app_a → depends on app_b
2. Resolve app_b → depends on app_c
3. Resolve app_c → depends on app_a
4. Build graph: app_a → app_b → app_c → app_a
5. Attempt topological sort → fails
6. Find strongly connected components → `[[app_a, app_b, app_c]]`
7. Return error

**Error**:

```
Dependency cycle(s) detected:
applications: app_a app_b app_c depend on each other
```

---

## Scenario 3: Profile-Specific Dependencies

**`rebar.config`**:

```erlang
{deps, [jsx]}.
{profiles, [
    {test, [{deps, [meck, proper]}]}
]}.
```

**Execution**: `rebar3 as test compile`

**Flow**:

1. Active profiles: `[default, test]`
2. Level 0, default profile: `[jsx]`
3. Level 0, test profile: `[meck, proper]`
4. Resolve all at level 0
5. Continue with transitive deps
6. Build graph with all apps
7. Topological sort
8. Write lock file with ONLY `jsx` (default profile only)

**`rebar.lock`**:

```erlang
{"1.2.0",
[{<<"jsx">>, {pkg, <<"jsx">>, <<"3.1.0">>}, 0}]}.
[...].
```

Note: `meck` and `proper` NOT in lock file

---

## Scenario 4: Using Checkout Dependencies

**Setup**:

```
my_app/
├── _checkouts/
│   └── my_dep/
│       ├── src/
│       └── rebar.config
├── rebar.config: {deps, [{my_dep, "1.0.0"}]}
└── src/
```

**Execution**: `rebar3 compile`

**Flow**:

1. Parse deps: `my_dep` version "1.0.0"
2. Discover checkout in `_checkouts/my_dep/`
3. Mark as checkout dependency
4. Skip fetching (use local version)
5. Resolve transitive deps from checkout's `rebar.config`
6. In locking stage: skip adding to lock file

**Output**:

```
App my_dep is a checkout dependency and cannot be locked.
```

**`rebar.lock`**: Does NOT include `my_dep`

---

## Scenario 5: Dependency Upgrade

**Initial `rebar.lock`**:

```erlang
{"1.2.0",
[{<<"jsx">>, {pkg, <<"jsx">>, <<"3.0.0">>}, 0}]}.
```

**Execution**: `rebar3 upgrade jsx`

**Flow**:

1. Set upgrade flag to `true`
2. Ignore locked version for `jsx`
3. Query Hex for latest version → "3.1.0"
4. Fetch new version
5. Resolve dependencies normally
6. Update lock file with new version

**New `rebar.lock`**:

```erlang
{"1.2.0",
[{<<"jsx">>, {pkg, <<"jsx">>, <<"3.1.0">>}, 0}]}.
```

---

## Scenario 6: Git Dependency with Transitive Deps

**`rebar.config`**:

```erlang
{deps, [
    {my_git_dep, {git, "https://github.com/user/my_git_dep.git", {tag, "v1.0.0"}}}
]}.
```

**`my_git_dep/rebar.config`**:

```erlang
{deps, [jsx]}.
```

**Flow**:

1. Resolve `my_git_dep` from Git
2. Fetch and clone the repository
3. Parse `my_git_dep/rebar.config`
4. Find transitive dependency: `jsx`
5. Resolve `jsx` from Hex
6. Build graph:

   ```
   my_app → [my_git_dep]
   my_git_dep → [jsx]
   jsx → []
   ```

7. Topological sort → `[jsx, my_git_dep, my_app]`

**`rebar.lock`**:

```erlang
{"1.2.0",
[{<<"jsx">>, {pkg, <<"jsx">>, <<"3.1.0">>}, 1},
 {<<"my_git_dep">>, {git, "https://github.com/user/my_git_dep.git",
                          {ref, "abc123..."}}, 0}]}.
```

Note: `jsx` is level 1 (transitive), `my_git_dep` is level 0 (direct)
