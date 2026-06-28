# Error Conditions

## Circular Dependency Detected

**Condition**: Dependency graph contains cycles

**Detection**: `digraph_utils:topsort/1` returns `false` and `digraph_utils:is_acyclic/1` returns `false`

**Error Format**:

```erlang
{cycles, [[<<"app1">>, <<"app2">>, <<"app3">>], ...]}
```

**Error Message**:

```
Dependency cycle(s) detected:
applications: app1 app2 app3 depend on each other
```

**Example**:

```
app1 depends on app2
app2 depends on app3
app3 depends on app1
→ Cycle: [app1, app2, app3]
```

**Recovery**: None; must break the cycle in dependencies

---

## Package Not Found in Registry

**Condition**: Hex package doesn't exist

**Error Format**:

```erlang
{missing_package, Package, Version}
{missing_package, Package}
```

**Error Message**:

```
Package not found in registry: jsx-3.1.0
Package not found in registry: nonexistent_package
```

**Recovery**: Fix package name or check Hex registry

---

## Bad Version Constraint

**Condition**: Invalid version specification

**Error Format**:

```erlang
{bad_constraint, Name, Constraint}
```

**Error Message**:

```
Unable to parse version for package jsx: ~~>3.1.0
```

**Recovery**: Fix version constraint syntax

---

## Package Not Buildable

**Condition**: Hex package exists but isn't rebar3-compatible

**Error Format**:

```erlang
{not_rebar_package, Package, Version}
```

**Error Message**:

```
Package not buildable with rebar3: old_package-1.0.0
```

**Recovery**: Use different version or different package

---

## Failed to Load Registry

**Condition**: Cannot download Hex registry

**Error Format**:

```erlang
{load_registry_fail, Dep}
```

**Error Message**:

```
Error loading registry to resolve version of jsx. Try fixing by running 'rebar3 update'
```

**Recovery**: Run `rebar3 update` or check network connection

---

## Dependency Application Not Found

**Condition**: Dependency fetched but application not in expected location

**Error Format**:

```erlang
{dep_app_not_found, AppDir, AppName}
```

**Error Message**:

```
Dependency failure: Application my_dep not found at the top level of directory /path/to/dep
```

**Possible Causes**:

- Dependency doesn't have `.app.src` file
- Application name doesn't match directory name
- Dependency is not an OTP application

**Recovery**: Fix dependency structure or use different version
