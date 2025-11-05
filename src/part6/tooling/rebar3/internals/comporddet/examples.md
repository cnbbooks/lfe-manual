# Example Scenarios

## Scenario 1: Simple Linear Dependencies

**Applications**:

```
my_app:
  - depends on: [jsx]
jsx:
  - depends on: [kernel, stdlib]
```

**Graph Construction**:

```
Vertices: my_app, jsx, kernel, stdlib
Edges:
  my_app → jsx
  jsx → kernel
  jsx → stdlib
```

**Topological Sort Result**:

```
[kernel, stdlib, jsx, my_app]
```

**After Reversal (compilation order)**:

```
[my_app, jsx, stdlib, kernel]  % Actually reversed already by topsort output
```

**Wait, correction**: topsort gives `[kernel, stdlib, jsx, my_app]`, we reverse to get `[my_app, jsx, stdlib, kernel]`?

**Actually**: topsort gives dependencies first, so we get `[kernel, stdlib, jsx, my_app]`, then we reverse to... wait, that doesn't make sense.

**Clarification from code**:

```erlang
V -> {ok, names_to_apps(lists:reverse(V), Apps)}
```

So if topsort returns `[my_app, jsx, stdlib, kernel]`, we reverse to `[kernel, stdlib, jsx, my_app]`.

**Correct Interpretation**:

- Topsort returns: `[my_app, jsx, stdlib, kernel]` (reverse topological order)
- We reverse to: `[kernel, stdlib, jsx, my_app]` (topological order - dependencies first)
- Final: `jsx`, then `my_app` (OTP apps filtered out by `names_to_apps`)

---

## Scenario 2: Diamond Dependencies

**Applications**:

```
my_app:
  - depends on: [web, db]
web:
  - depends on: [cowboy, common]
db:
  - depends on: [epgsql, common]
common:
  - depends on: [kernel, stdlib]
```

**Graph**:

```
Vertices: my_app, web, db, cowboy, epgsql, common, kernel, stdlib
Edges:
  my_app → web
  my_app → db
  web → cowboy
  web → common
  db → epgsql
  db → common
  common → kernel
  common → stdlib
```

**Possible Topological Sorts** (dependencies first):

```
[kernel, stdlib, common, cowboy, epgsql, web, db, my_app]
[kernel, stdlib, common, epgsql, cowboy, web, db, my_app]
[kernel, stdlib, common, cowboy, web, epgsql, db, my_app]
... (many valid orderings)
```

**Key Property**: `common` always before `web` and `db`; `web` and `db` always before `my_app`

**After Filtering OTP Apps**:

```
[common, cowboy, epgsql, web, db, my_app]
```

---

## Scenario 3: Circular Dependency

**Applications**:

```
app_a:
  - depends on: [app_b]
app_b:
  - depends on: [app_c]
app_c:
  - depends on: [app_a]
```

**Graph**:

```
Vertices: app_a, app_b, app_c
Edges:
  app_a → app_b
  app_b → app_c
  app_c → app_a
```

**Topological Sort**: Fails

**Cycle Detection**:

```erlang
digraph_utils:strong_components(Graph)
% Returns: [[app_a, app_b, app_c], ...other components...]

Filter length > 1: [[app_a, app_b, app_c]]
```

**Error**:

```erlang
{error, {cycles, [[<<"app_a">>, <<"app_b">>, <<"app_c">>]]}}
```

**User Message**:

```
Dependency cycle(s) detected:
applications: app_a app_b app_c depend on each other
```

---

## Scenario 4: Multiple Independent Cycles

**Applications**:

```
Group 1:
  app1 → app2 → app3 → app1

Group 2:
  app4 → app5 → app4

Group 3:
  app6 (no cycle, depends on kernel only)
```

**Cycle Detection**:

```erlang
Strongly connected components:
[[app1, app2, app3], [app4, app5], [app6], [kernel], [stdlib]]

Filter length > 1:
[[app1, app2, app3], [app4, app5]]
```

**Error**:

```erlang
{error, {cycles, [[<<"app1">>, <<"app2">>, <<"app3">>],
                  [<<"app4">>, <<"app5">>]]}}
```

**User Message**:

```
Dependency cycle(s) detected:
applications: app1 app2 app3 depend on each other
applications: app4 app5 depend on each other
```

---

## Scenario 5: Dependencies vs Project Apps

**Project Structure**:

```
Project Apps:
  - my_web
  - my_db

Dependencies:
  - cowboy
  - epgsql
  - ranch
```

**Dependency Compilation Order**:

1. Resolve all dependencies (cowboy, epgsql, ranch)
2. Determine order: `[ranch, cowboy, epgsql]` (ranch is cowboy's dep)
3. Call `cull_compile([ranch, cowboy, epgsql], [my_web, my_db])`
4. Result: `[ranch, cowboy, epgsql]` (project apps already removed)
5. Compile in that order

**Project App Compilation Order**:

1. Get project apps: `[my_web, my_db]`
2. Determine order: Check if my_web depends on my_db or vice versa
3. If my_web depends on my_db: `[my_db, my_web]`
4. If independent: arbitrary order, e.g., `[my_web, my_db]`
5. Compile in that order

---

## Scenario 6: Build vs Runtime Dependencies

**my_app.app.src**:

```erlang
{applications, [kernel, stdlib, cowboy]}.  % Runtime deps
```

**rebar.config**:

```erlang
{deps, [{parse_trans, "3.3.0"}]}.  % Build-time only
```

**Dependency Graph**:

- Includes both `cowboy` (runtime) and `parse_trans` (build-time)
- `all_apps_deps/1` unions both sources
- Result: `[cowboy, kernel, parse_trans, stdlib]`

**Compilation Order**:

```
[parse_trans, cowboy, my_app]
```

**Why**: parse_trans might be needed during my_app compilation (as parse transform), cowboy needed for runtime

---

## Scenario 7: Umbrella Project with Inter-App Dependencies

**Project Structure**:

```
apps/
  ├── common/     (no deps)
  ├── api/        (depends on common)
  └── web/        (depends on common, api)
```

**Compilation Order Determination**:

```erlang
Input: [common, api, web]

Graph:
  common → [kernel, stdlib]
  api → [common, kernel, stdlib]
  web → [common, api, kernel, stdlib]

Sorted: [common, api, web]
```

**Result**: common compiled first, then api, then web

**Critical**: Without proper ordering, web would fail to compile due to missing common and api
