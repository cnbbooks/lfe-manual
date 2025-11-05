# Functions & API Calls

## `rebar_compiler_dag:init/4`

**Purpose**: Initialize or restore DAG from disk

**Signature**:

```erlang
-spec init(Dir, Compiler, Label, CritMeta) -> Graph when
    Dir :: file:filename_all(),
    Compiler :: atom(),
    Label :: string() | undefined,
    CritMeta :: term(),
    Graph :: digraph:graph().
```

**Arguments**:

- `Dir`: Directory for DAG storage (typically deps_dir)
- `Compiler`: Compiler module name (e.g., `rebar_compiler_erl`)
- `Label`: Optional label (e.g., "apps", "project_apps")
- `CritMeta`: Critical metadata for invalidation (compiler version, etc.)

**Returns**: Digraph handle

**Flow**:

1. Create new acyclic digraph
2. Determine DAG file path
3. Try to restore from file
4. If restoration fails: delete invalid file, return empty graph
5. Return graph

**DAG File Path**:

- With label: `_build/PROFILE/.rebar3/COMPILER/source_LABEL.dag`
- Without label: `_build/PROFILE/.rebar3/COMPILER/source.dag`

---

## `rebar_compiler_dag:status/4`

**Purpose**: Quick validation of DAG file without loading

**Signature**:

```erlang
-spec status(Dir, Compiler, Label, CritMeta) -> Status when
    Dir :: file:filename_all(),
    Compiler :: atom(),
    Label :: string() | undefined,
    CritMeta :: term(),
    Status :: valid | bad_format | bad_vsn | bad_meta | not_found.
```

**Returns**:

- `valid`: DAG file valid and compatible
- `bad_format`: File corrupted or wrong format
- `bad_vsn`: DAG version mismatch (current: 4)
- `bad_meta`: Critical metadata mismatch
- `not_found`: DAG file doesn't exist

**Use**: Determines if full rebuild needed before actual compilation

---

## `rebar_compiler_dag:prune/5`

**Purpose**: Remove deleted files from DAG

**Signature**:

```erlang
-spec prune(G, SrcExt, ArtifactExt, Sources, AppPaths) -> ok when
    G :: digraph:graph(),
    SrcExt :: string(),
    ArtifactExt :: [string()],
    Sources :: [file:filename()],
    AppPaths :: [{AppDir, OutDir}].
```

**Arguments**:

- `G`: DAG graph
- `SrcExt`: Source extension (e.g., ".erl")
- `ArtifactExt`: Artifact extensions (e.g., [".beam"])
- `Sources`: Current list of source files
- `AppPaths`: App directories and output directories

**Flow**:

1. Find vertices not in `Sources`
2. For source files: check if in app paths, delete if so
3. For header files with no incoming edges: delete
4. For deleted sources: delete associated artifacts
5. Mark DAG dirty if anything deleted

---

## `rebar_compiler_dag:populate_sources/5`

**Purpose**: Add sources and scan their dependencies

**Signature**:

```erlang
-spec populate_sources(G, Compiler, InDirs, Sources, DepOpts) -> ok when
    G :: digraph:graph(),
    Compiler :: module(),
    InDirs :: [file:filename()],
    Sources :: [file:filename()],
    DepOpts :: term().
```

**Arguments**:

- `G`: DAG graph
- `Compiler`: Compiler module (for `dependencies/4` callback)
- `InDirs`: Include directories to search
- `Sources`: Source files to add
- `DepOpts`: Options for dependency scanning

**Flow**:

1. Create parallel worker pool
2. For each source:
   - Check if vertex exists
   - Compare timestamps
   - If new or modified: queue dependency scan
   - If unchanged: skip
3. Wait for all scans to complete
4. Add dependency edges to graph
5. Delete obsolete edges
6. Mark dirty if changes made

**Parallel Scanning**: Uses `rebar_parallel` for performance

---

## `rebar_compiler_dag:populate_deps/3`

**Purpose**: Scan header files for timestamp updates

**Signature**:

```erlang
-spec populate_deps(G, SourceExt, ArtifactExts) -> ok when
    G :: digraph:graph(),
    SourceExt :: string(),
    ArtifactExts :: [string()].
```

**Flow**:

1. Find all vertices that are neither sources nor artifacts (headers)
2. Refresh timestamp for each header file
3. Update vertex labels

---

## `rebar_compiler_dag:propagate_stamps/1`

**Purpose**: Propagate timestamps through dependency chains

**Signature**:

```erlang
-spec propagate_stamps(G :: digraph:graph()) -> ok.
```

**Algorithm**:

```
Given: A → B → C → D with timestamps [0, 1, 3, 2]
Process in reverse topological order: [D, C, B, A]

For D (timestamp 2):
  Dependencies: none
  Keep timestamp: 2

For C (timestamp 3):
  Dependencies: [D with timestamp 2]
  Max dependency: 2
  Current: 3
  Keep timestamp: 3 (already newer)

For B (timestamp 1):
  Dependencies: [C with timestamp 3]
  Max dependency: 3
  Current: 1 < 3
  Update timestamp: 3

For A (timestamp 0):
  Dependencies: [B with timestamp 3]
  Max dependency: 3
  Current: 0 < 3
  Update timestamp: 3

Result: [3, 3, 3, 2]
```

**Why**: Ensures dependent files know when their dependencies changed

---

## `rebar_compiler_dag:compile_order/4`

**Purpose**: Determine inter-application compilation order

**Signature**:

```erlang
-spec compile_order(G, AppDefs, SrcExt, ArtifactExt) -> [AppName] when
    G :: digraph:graph(),
    AppDefs :: [{AppName, AppPath}],
    SrcExt :: string(),
    ArtifactExt :: [string()],
    AppName :: atom().
```

**Returns**: Ordered list of application names

**Flow**:

1. Create new app-level DAG
2. For each file dependency edge in G:
   - Skip artifact edges
   - Resolve both files to apps
   - If different apps: add inter-app edge
3. Use interleave sort (respects original order + DAG constraints)
4. Return sorted app names
5. Delete app DAG

**Interleave Sort**: Preserves `rebar.config` order while respecting hard dependencies

---

## `rebar_compiler_dag:store_artifact/4`

**Purpose**: Track compiled artifact in DAG

**Signature**:

```erlang
-spec store_artifact(G, Source, Target, Meta) -> ok when
    G :: digraph:graph(),
    Source :: file:filename(),
    Target :: file:filename(),
    Meta :: term().
```

**Arguments**:

- `G`: DAG graph
- `Source`: Source file path
- `Target`: Artifact file path
- `Meta`: Artifact metadata (options, versions)

**Flow**:

1. Add artifact vertex with metadata
2. Add artifact edge: `Target → Source`
3. Check for duplicate edge (artifact compilation may run multiple times)
4. Mark DAG dirty

---

## `rebar_compiler_dag:maybe_store/5`

**Purpose**: Save DAG to disk if modified

**Signature**:

```erlang
-spec maybe_store(G, Dir, Compiler, Label, CritMeta) -> ok when
    G :: digraph:graph(),
    Dir :: file:filename_all(),
    Compiler :: atom(),
    Label :: string() | undefined,
    CritMeta :: term().
```

**Flow**:

1. Check if DAG is dirty
2. If dirty:
   - Clear dirty bit
   - Serialize DAG to binary
   - Write to file
3. If not dirty: skip save

**DAG File Format**:

```erlang
#dag{
    vsn = 4,
    meta = CritMeta,
    vtab = Vertices,
    etab = Edges,
    ntab = NeighborTable
}
```

---

## `rebar_compiler_dag:terminate/1`

**Purpose**: Clean up in-memory DAG

**Signature**:

```erlang
-spec terminate(G :: digraph:graph()) -> true.
```

**Flow**: Calls `digraph:delete/1`

**Note**: Does not delete disk files
