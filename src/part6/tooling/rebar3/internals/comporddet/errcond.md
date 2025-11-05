# Error Conditions

## Circular Dependencies Detected

**Condition**: Dependency graph contains one or more cycles

**Detection**: `digraph_utils:is_acyclic/1` returns `false`

**Error Format**:

```erlang
{cycles, [[<<"app1">>, <<"app2">>, <<"app3">>], [<<"app4">>, <<"app5">>]]}
```

**Error Message** (from `rebar_prv_install_deps`):

```
Dependency cycle(s) detected:
applications: app1 app2 app3 depend on each other
applications: app4 app5 depend on each other
```

**Example Cycle**:

```
app1 depends on app2
app2 depends on app3
app3 depends on app1
```

**Recovery**: None; must break the cycle in dependencies

**Common Causes**:

- Circular `applications` lists in `.app` files
- Circular `deps` in `rebar.config` files
- Mix of runtime and build-time circular dependencies

---

## Topological Sort Failed (Non-Cyclic)

**Condition**: Graph is acyclic but sort fails anyway

**Error Format**:

```erlang
{error, no_sort}
```

**Likelihood**: Very rare; theoretical edge case

**Possible Causes**:

- Erlang digraph library issue
- Malformed graph structure

**Recovery**: Report as bug

---

## Missing Dependency in Graph

**Condition**: Application references dependency not in graph

**Behavior**: Not an error at this stage

**Handling**:

- Digraph adds vertex automatically for missing deps
- Vertex exists but no app info record associated
- `names_to_apps/2` skips missing apps
- Error likely caught in earlier stage (dependency resolution)
