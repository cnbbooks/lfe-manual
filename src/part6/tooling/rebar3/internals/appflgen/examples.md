# Example Scenarios

## Scenario 1: Simple Application

**.app.src**:

```erlang
{application, my_app, [
    {description, "My Application"},
    {vsn, "1.0.0"},
    {registered, []},
    {applications, [kernel, stdlib]},
    {mod, {my_app_app, []}},
    {modules, []}
]}.
```

**After Compilation**:

- `ebin/my_app_app.beam` exists
- `ebin/my_app_sup.beam` exists

**Generated .app**:

```erlang
{application, my_app, [
    {description, "My Application"},
    {vsn, "1.0.0"},
    {registered, []},
    {applications, [kernel, stdlib]},
    {mod, {my_app_app, []}},
    {modules, [my_app_app, my_app_sup]}
]}.
```

---

## Scenario 2: Git-Based Version

**.app.src**:

```erlang
{application, my_app, [
    {description, "My Application"},
    {vsn, git},
    {applications, [kernel, stdlib]},
    {modules, []}
]}.
```

**Git State**: Tag `v1.2.3`, 5 commits ahead

**Generated .app**:

```erlang
{application, my_app, [
    {description, "My Application"},
    {vsn, "1.2.3-5-gabc123"},
    {applications, [kernel, stdlib]},
    {modules, [my_app_app]}
]}.
```

---

## Scenario 3: Variable Substitution

**rebar.config**:

```erlang
{app_vars_file, "vars/build.vars"}.
```

**vars/build.vars**:

```erlang
{build_time, "2024-01-15T10:30:00Z"}.
{build_user, "jenkins"}.
```

**.app.src**:

```erlang
{application, my_app, [
    {vsn, "1.0.0"},
    {applications, [kernel, stdlib]},
    {modules, []},
    {env, [
        {build_time, build_time},
        {build_user, build_user}
    ]}
]}.
```

**Generated .app**:

```erlang
{application, my_app, [
    {vsn, "1.0.0"},
    {applications, [kernel, stdlib]},
    {modules, [my_app_app]},
    {env, [
        {build_time, "2024-01-15T10:30:00Z"},
        {build_user, "jenkins"}
    ]}
]}.
```

---

## Scenario 4: Extra Source Directories

**rebar.config**:

```erlang
{extra_src_dirs, ["test"]}.
```

**Files**:

- `src/my_app.erl` → `ebin/my_app.beam`
- `test/my_test.erl` → `_build/test/extras/test/my_test.beam`

**Generated .app** (modules list):

```erlang
{modules, [my_app]}  % my_test NOT included
```

---

## Scenario 5: Dynamic .app.src.script

**File**: `src/my_app.app.src.script`

```erlang
%% Dynamic version based on environment
Version = case os:getenv("RELEASE_VERSION") of
    false -> "dev";
    V -> V
end,

%% Dynamic applications based on profile
ExtraApps = case os:getenv("PROFILE") of
    "prod" -> [recon];
    _ -> []
end,

{application, my_app, [
    {description, "My App"},
    {vsn, Version},
    {applications, [kernel, stdlib | ExtraApps]},
    {modules, []}
]}.
```

**Production Build** (`RELEASE_VERSION=1.0.0, PROFILE=prod`):

```erlang
{application, my_app, [
    {description, "My App"},
    {vsn, "1.0.0"},
    {applications, [kernel, stdlib, recon]},
    {modules, [my_app]}
]}.
```

**Development Build** (no env vars):

```erlang
{application, my_app, [
    {description, "My App"},
    {vsn, "dev"},
    {applications, [kernel, stdlib]},
    {modules, [my_app]}
]}.
```
