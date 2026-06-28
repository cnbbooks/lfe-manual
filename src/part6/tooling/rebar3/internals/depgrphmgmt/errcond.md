# Error Conditions

## DAG Restoration Failure

**Condition**: Corrupted or incompatible DAG file

**Handling**:

- Log warning
- Delete corrupt file
- Create new empty DAG
- Full rebuild triggered

**Not Fatal**: Compilation continues

---

## Version Mismatch

**Condition**: DAG file has different version number

**Handling**:

- DAG discarded
- Full rebuild

**Common Cause**: rebar3 upgrade

---

## Critical Metadata Mismatch

**Condition**: Compiler version or important options changed

**Example**: Erlang/OTP upgraded from 24 to 25

**Handling**:

- DAG discarded
- Full rebuild

**Ensures**: Artifacts always match current environment

---

## Circular Dependencies (File-Level)

**Condition**: Files depend on each other in cycle

**Detection**: `digraph_utils:topsort/1` fails during timestamp propagation

**Handling**:

- Timestamps may not propagate correctly
- Compilation may fail later
- Not specifically caught at DAG level
