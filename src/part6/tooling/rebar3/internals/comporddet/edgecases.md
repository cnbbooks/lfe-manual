# Edge Cases

## Empty Application List

**Input**: `[]`

**Behavior**: Returns `{ok, []}`

**Use Case**: No applications to compile

---

## Single Application

**Input**: Single app with no dependencies

**Behavior**: Returns `{ok, [App]}`

**Graph**: Single vertex, no edges

---

## Application Depends on Itself

**Behavior**: Creates self-loop in graph

**Detection**: Caught by cycle detection

**Error**: Reported as single-app cycle

---

## Transitive Dependencies

**Scenario**:

```
my_app → lib_a → lib_b → lib_c
```

**Behavior**: All transitive dependencies included in graph

**Ordering**: `[lib_c, lib_b, lib_a, my_app]`

**Why It Works**: Each level adds its dependencies to graph

---

## Diamond Dependency Pattern

**Scenario**:

```
my_app → [lib_a, lib_b]
lib_a → lib_common
lib_b → lib_common
```

**Behavior**: `lib_common` appears once in sorted output

**Ordering**: `[lib_common, lib_a, lib_b, my_app]` (or `lib_b` before `lib_a`)

**Note**: Order between `lib_a` and `lib_b` is arbitrary (no dependency between them)

---

## Dependency Referenced but Not Present

**Scenario**: App lists dependency that wasn't resolved

**Example**:

```erlang
% my_app.app.src
{applications, [kernel, stdlib, missing_lib]}
```

**Behavior**:

- Vertex created for `missing_lib`
- No corresponding app info
- `names_to_apps` skips it
- Won't appear in final sorted list
- Compilation will likely fail later

---

## OTP Applications in Dependency List

**Scenario**: Dependencies include OTP apps like `kernel`, `stdlib`

**Behavior**: Included in graph but not in final compile list

**Reason**: OTP apps aren't in the input app list, so `names_to_apps` skips them

**Example**:

```erlang
Input: [my_app]
my_app depends on: [kernel, stdlib, jsx]

Graph vertices: my_app, kernel, stdlib, jsx
Sorted: [kernel, stdlib, jsx, my_app]
names_to_apps result: [jsx, my_app]  % kernel, stdlib not in input
```

---

## Multiple Independent Apps

**Scenario**: Multiple apps with no inter-dependencies

**Example**:

```
app1 → [kernel, stdlib]
app2 → [kernel, stdlib]
app3 → [kernel, stdlib]
```

**Behavior**: Arbitrary order between app1, app2, app3

**Ordering**: Any permutation valid, e.g., `[app1, app2, app3]` or `[app3, app1, app2]`
