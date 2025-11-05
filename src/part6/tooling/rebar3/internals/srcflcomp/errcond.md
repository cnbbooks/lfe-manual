# Error Conditions

## Compilation Error

**Condition**: Source file has syntax or semantic errors

**Error Format** (from `compile:file/2`):

```erlang
{error, [{File, [{Line, Module, Description}]}], Warnings}
```

**Example**:

```
Compiling src/my_module.erl failed
src/my_module.erl:10: syntax error before: '}'
```

**Recovery**: Fix source file and recompile

---

## Missing Include File

**Condition**: `-include("file.hrl")` file not found

**Error**:

```
src/my_module.erl:5: can't find include file "missing.hrl"
```

**Common Causes**:

- File doesn't exist
- Not in include path
- Typo in filename

**Recovery**: Add file or fix include path

---

## Parse Transform Not Found

**Condition**: Parse transform module not compiled or not in path

**Error**:

```
src/my_module.erl:1: parse transform 'my_transform' undefined
```

**Recovery**:

- Ensure parse transform compiled first
- Add to `erl_first_files` if needed
- Check module name spelling

---

## Circular Dependencies

**Condition**: Files depend on each other in a cycle

**Detection**: DAG topological sort fails

**Example**:

```
mod_a.erl includes mod_b.hrl
mod_b.erl includes mod_a.hrl
```

**Impact**: Compilation may fail or produce incorrect results

**Recovery**: Refactor to break cycle

---

## DAG Version Mismatch

**Condition**: Cached DAG from different rebar3/compiler version

**Behavior**: DAG discarded, full recompilation

**Log**: Debug message about DAG invalidation

---

## Missing Artifact

**Condition**: Expected artifact not created after compilation

**Error**:

```
Missing artifact path/to/expected/file.beam
```

**Causes**:

- Compiler failed silently
- Custom artifact configuration incorrect

**Recovery**: Check compiler output, fix artifact configuration
