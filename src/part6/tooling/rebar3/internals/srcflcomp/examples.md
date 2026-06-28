# Example Scenarios

## Scenario 1: Simple Erlang Module Compilation

**Files**:

```
src/my_module.erl
```

**Flow**:

1. Compiler: `rebar_compiler_erl`
2. Find: `[src/my_module.erl]`
3. Check DAG: not compiled yet
4. Needed files: `[src/my_module.erl]`
5. No first files, no dependencies
6. Parallel compilation: single file, no benefit
7. Compile: `compile:file("src/my_module.erl", [{outdir, "ebin"}, ...])`
8. Result: `ebin/my_module.beam`

---

## Scenario 2: Module with Include Files

**Files**:

```
src/my_module.erl
include/types.hrl
```

**my_module.erl**:

```erlang
-module(my_module).
-include("types.hrl").
```

**Flow**:

1. Dependency scan finds `include/types.hrl`
2. Add dependency: `my_module.erl` → `types.hrl`
3. Store in DAG
4. Compile `my_module.erl`
5. Store artifact with timestamp

**Next Run**:

1. Check timestamps
2. If `types.hrl` modified: recompile `my_module.erl`
3. If not: skip compilation

---

## Scenario 3: Parse Transform

**Files**:

```
src/my_transform.erl  (parse transform)
src/user_module.erl   (uses transform)
```

**user_module.erl**:

```erlang
-compile([{parse_transform, my_transform}]).
```

**Flow**:

1. Split sources: `[my_transform.erl]` vs `[user_module.erl]`
2. FirstFiles: `[my_transform.erl]`
3. Compile `my_transform.erl` first
4. Dependency: `user_module.erl` → `my_transform.erl`
5. Compile `user_module.erl` (transform applied)

---

## Scenario 4: Yecc Parser

**Files**:

```
src/parser.yrl
```

**Flow**:

1. Compiler: `rebar_compiler_yrl`
2. Find: `[src/parser.yrl]`
3. Compile: `yecc:file("src/parser.yrl")`
4. Generates: `src/parser.erl`
5. Compiler: `rebar_compiler_erl`
6. Find: `[src/parser.erl]` (generated)
7. Compile: `compile:file("src/parser.erl", ...)`
8. Result: `ebin/parser.beam`

---

## Scenario 5: Parallel Compilation

**Files**:

```
src/mod_a.erl  (independent)
src/mod_b.erl  (independent)
src/mod_c.erl  (independent)
src/mod_d.erl  (uses mod_a)
```

**Dependencies**:

- mod_d → mod_a
- mod_a, mod_b, mod_c independent

**Compilation Order**:

```
Sequential: [mod_a]  (mod_d depends on it)
Parallel: [mod_b, mod_c]  (independent)
Then Sequential: [mod_d]  (after mod_a done)
```

**Workers**: 2 parallel workers

1. Worker 1: compile mod_b
2. Worker 2: compile mod_c
3. Main: compile mod_a (sequential)
4. Main: compile mod_d (after mod_a)

---

## Scenario 6: Incremental Build

**Initial Build**:

```
Compile: mod_a, mod_b, mod_c
DAG: stored with timestamps
```

**Modify mod_b.erl**:

```
Find: mod_b.erl timestamp changed
DAG: mod_b needs recompilation
Compile: only mod_b
Result: 1 file compiled vs 3 initial
```

**Modify include/common.hrl** (used by mod_a, mod_c):

```
DAG propagation: common.hrl timestamp updated
Dependents: mod_a, mod_c use common.hrl
Compile: mod_a, mod_c (mod_b unchanged)
Result: 2 files compiled
```

---

## Scenario 7: Changed Compiler Options

**Initial**:

```erlang
{erl_opts, [debug_info]}.
```

**Build**: All files compiled with `debug_info`

**Modified**:

```erlang
{erl_opts, [debug_info, warnings_as_errors]}.
```

**Next Build**:

1. DAG compares stored opts vs current opts
2. Mismatch detected: `warnings_as_errors` added
3. All files need recompilation
4. Compile all with new options
5. Update DAG with new options
