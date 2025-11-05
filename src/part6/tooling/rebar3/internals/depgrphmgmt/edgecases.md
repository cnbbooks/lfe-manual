# Edge Cases

## Header File Deleted

**Scenario**: `.hrl` file removed from project

**Handling**:

1. `prune/5` finds header with no incoming edges
2. Delete vertex
3. Files including it will fail compilation (caught later)

---

## Source File Deleted

**Scenario**: `.erl` file removed

**Handling**:

1. `prune/5` finds source not in current list
2. Check if in app paths
3. Delete source vertex
4. Find and delete associated artifact (`.beam`)
5. Mark dirty

---

## Timestamp Regression

**Scenario**: File restored from backup with older timestamp

**Behavior**: Change not detected

**Limitation**: Timestamp-based system can't detect regression

**Workaround**: Force rebuild (`rebar3 clean && rebar3 compile`)

---

## Parse Transform Affects All Files

**Scenario**: Parse transform modified

**Handling**:

- Compiler (`rebar_compiler_erl`) detects parse transform change
- Returns all files as needing compilation
- DAG not specifically involved

---

## Multiple Artifacts from One Source

**Scenario**: `.mib` generates `.bin` and `.hrl`

**Handling**:

- Call `store_artifact/4` twice
- Two artifact vertices
- Two artifact edges to same source

---

## Concurrent Builds

**Scenario**: Two rebar3 processes running simultaneously

**Risk**: DAG file corruption

**Mitigation**: None built-in; avoid concurrent builds

---

## Large Projects

**Performance**: DAG operations scale linearly

**Optimization**: Parallel dependency scanning

**Typical**: 1000+ files handled efficiently
