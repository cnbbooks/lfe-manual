# Edge Cases

## Parse Transform Recompilation

**Scenario**: Parse transform source changed

**Behavior**: ALL files recompiled (parse transforms affect all files)

**Detection**: `needed_files/6` checks parse transform timestamps

---

## ERL_OPTS Change

**Condition**: Compiler options changed in `rebar.config`

**Behavior**: Affected files recompiled

**Detection**: DAG compares stored options with current options

---

## Include File Modified

**Condition**: `.hrl` file changed

**Behavior**: All files including it (directly or transitively) recompiled

**Detection**: DAG timestamp propagation

---

## Generated Source Files

**Scenario**: `.yrl` generates `.erl`, which must then compile to `.beam`

**Handling**:

1. `rebar_compiler_yrl` runs first, generates `.erl`
2. `rebar_compiler_erl` runs second, finds generated `.erl`
3. Compiles generated `.erl` to `.beam`

**Critical**: Compiler order matters

---

## Parallel Compilation Failures

**Scenario**: One of parallel compilations fails

**Behavior**:

- Worker reports error
- Main process collects error
- Compilation aborts with error

**Impact**: Some files may be partially compiled

---

## Large Projects

**Optimization**: Parallel compilation significantly speeds up builds

**Example**: 100 independent `.erl` files

- Sequential: ~100 seconds
- Parallel (8 cores): ~15 seconds
