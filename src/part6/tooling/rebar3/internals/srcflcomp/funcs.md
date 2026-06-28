# Functions & API Calls

## `rebar_prv_compile:compile/4`

**Purpose**: Main compilation orchestrator

**Signature**:

```erlang
-spec compile(State, Providers, Apps, Tag) -> [rebar_app_info:t()] when
    State :: rebar_state:t(),
    Providers :: [providers:t()],
    Apps :: [rebar_app_info:t()],
    Tag :: atom() | undefined.
```

**Arguments**:

- `State`: Current rebar state
- `Providers`: Registered providers (for hooks)
- `Apps`: Applications to compile
- `Tag`: DAG label (`apps`, `project_apps`, or `undefined`)

**Returns**: List of compiled app infos

**Flow**: Executes all sub-stages listed in overview

---

## `run_compilers/4`

**Purpose**: Execute all compiler modules for given applications

**Signature**:

```erlang
-spec run_compilers(State, Providers, Apps, Tag) -> [rebar_app_info:t()] when
    State :: rebar_state:t(),
    Providers :: [providers:t()],
    Apps :: [rebar_app_info:t()],
    Tag :: atom() | undefined.
```

**Flow**:

1. Load DAGs for all compilers via `load_dags/2`
2. Call `build_apps/3` with DAGs and Apps
3. Store DAGs to disk
4. Terminate DAGs
5. Return Apps unchanged

---

## `load_dags/2`

**Purpose**: Initialize or restore dependency graphs for all compilers

**Signature**:

```erlang
-spec load_dags(State, Tag) -> [{Module, {Graph, Metadata}}] when
    State :: rebar_state:t(),
    Tag :: atom() | undefined,
    Module :: module(),
    Graph :: digraph:graph(),
    Metadata :: {Dir, Label, CritMeta}.
```

**Returns**: List of `{CompilerModule, {DAG, {Dir, Label, CritMeta}}}` tuples

**Flow**:

1. Get compiler modules from state
2. For each compiler:
   - Determine DAG directory (deps_dir)
   - Determine DAG label from Tag
   - Build critical metadata (e.g., compiler version)
   - Call `rebar_compiler_dag:init/4` to load or create DAG
3. Return list of DAG info

---

## `build_rebar3_apps/3`

**Purpose**: Compile applications using rebar3 compilation system

**Signature**:

```erlang
-spec build_rebar3_apps(DAGs, Apps, State) -> ok when
    DAGs :: [{Module, digraph:graph()}],
    Apps :: [rebar_app_info:t()],
    State :: rebar_state:t().
```

**Flow**:

1. For each compiler (DAG):
   - Call `rebar_compiler:analyze_all/2`
   - Get compilation context and reordered apps
   - For each app in order:
     - Print "Compiling APP_NAME"
     - Call `rebar_compiler:compile_analyzed/3`
   - Handle extra_src_dirs separately
2. Return ok

---

## `rebar_compiler:analyze_all/2`

**Purpose**: Analyze all applications to build dependency information

**Signature**:

```erlang
-spec analyze_all(DAG, Apps) -> {Context, ReorderedApps} when
    DAG :: {Module, Graph},
    Apps :: [rebar_app_info:t()],
    Context :: map(),
    ReorderedApps :: [rebar_app_info:t()].
```

**Flow**:

1. Prepare compiler environment (add paths)
2. Gather contexts for all apps via `Compiler:context/1`
3. For each app:
   - Find source files
   - Populate DAG with sources via `rebar_compiler_dag:populate_sources/5`
4. Prune deleted files from DAG
5. Populate dependencies via `rebar_compiler_dag:populate_deps/3`
6. Propagate timestamps via `rebar_compiler_dag:propagate_stamps/1`
7. Determine inter-app compile order
8. Return context and reordered apps

---

## `rebar_compiler:compile_analyzed/3`

**Purpose**: Compile a single application using pre-analyzed context

**Signature**:

```erlang
-spec compile_analyzed(DAG, AppInfo, Context) -> ok when
    DAG :: {Module, Graph},
    AppInfo :: rebar_app_info:t(),
    Context :: map().
```

**Flow**: Calls `run/4` to execute compilation

---

## `run/4` (internal to rebar_compiler)

**Purpose**: Execute compiler for one application

**Signature**:

```erlang
-spec run(Graph, CompilerMod, AppInfo, Contexts) -> ok when
    Graph :: digraph:graph(),
    CompilerMod :: module(),
    AppInfo :: rebar_app_info:t(),
    Contexts :: map().
```

**Flow**:

1. Get compiler context for this app
2. Find source files
3. Call `CompilerMod:needed_files/4`
4. Split into `FirstFiles` and `RestFiles`
5. Compile FirstFiles sequentially via `compile_each/5`
6. Check if RestFiles is `{Sequential, Parallel}` or flat list
7. If split: compile Sequential, then Parallel
8. If flat: compile all sequentially
9. Store artifacts in DAG via `store_artifacts/2`

---

## `rebar_compiler_erl:context/1`

**Purpose**: Provide Erlang compiler configuration

**Returns**:

```erlang
#{src_dirs => ExistingSrcDirs,
  include_dirs => AbsIncl,
  src_ext => ".erl",
  out_mappings => [{".beam", EbinDir}],
  dependencies_opts => [{includes, AbsIncl},
                        {macros, Macros},
                        {parse_transforms, PTrans}]}
```

**Include Directories** (in order):

1. `APP/include/` (standard)
2. Directories from `{i, Dir}` in `erl_opts`
3. All source directories (including recursive)
4. Top-level app directory

---

## `rebar_compiler_erl:needed_files/4`

**Purpose**: Determine which `.erl` files need compilation

**Flow**:

1. Split sources into parse transforms vs rest
2. Check if any parse transform needs recompilation
3. If yes: recompile ALL files (parse transforms affect everything)
4. If no: determine needed files via `needed_files/6`
5. Extract `erl_first_files` configuration
6. Build subgraph of needed files
7. Topological sort the subgraph
8. Partition into files with incoming deps (sequential) vs independent (parallel)
9. Return `{{FirstFiles, FirstOpts}, {{Sequential, Parallel}, RestOpts}}`

---

## `rebar_compiler_erl:dependencies/4`

**Purpose**: Find dependencies for an `.erl` file

**Uses**: `rebar_compiler_epp` - enhanced Erlang preprocessor

**Returns**: List of dependency file paths

**Dependencies Include**:

- `-include("file.hrl")` directives
- `-include_lib("app/include/file.hrl")` directives
- `-parse_transform(module)` directives → module source file
- `-behaviour(module)` directives → behavior source file

---

## `rebar_compiler_erl:compile/4`

**Purpose**: Compile single `.erl` file to `.beam`

**Signature**:

```erlang
-spec compile(Source, Mappings, Config, ErlOpts) -> Result when
    Source :: file:filename(),
    Mappings :: [{Ext, Dir}],
    Config :: rebar_dict(),
    ErlOpts :: [term()],
    Result :: ok | {ok, Warnings} | error | {error, Errors, Warnings}.
```

**Flow**:

1. Extract output directory from Mappings
2. Build options: `[{outdir, OutDir}, no_spawn_compiler_process | ErlOpts]`
3. Call `compile:file(Source, Options)`
4. Handle result:
   - `{ok, Mod}`: Success
   - `{ok, Mod, Warnings}`: Success with warnings
   - `{error, Errors, Warnings}`: Compilation failed
5. Format errors/warnings for display
6. Return result

---

## `rebar_compiler_erl:compile_and_track/4`

**Purpose**: Compile and track artifact metadata

**Returns**:

```erlang
{ok, [{Source, Target, AllOpts}]}
{ok, [{Source, Target, AllOpts}], Warnings}
{error, Errors, Warnings}
```

**Tracked Data**:

- Source file path
- Target artifact path
- Complete compilation options (for change detection)

**Used By**: DAG system to detect option changes requiring recompilation

---

## `compile_each/5` (internal)

**Purpose**: Compile list of files sequentially

**Flow**:

- For each file:
  - Check if compiler exports `compile_and_track/4`
  - If yes: call and accumulate artifacts
  - If no: call `compile/4` (no tracking)
- Return list of tracked artifacts

---

## `compile_parallel/5` (internal)

**Purpose**: Compile list of files in parallel

**Uses**: `rebar_parallel:queue/4`

**Flow**:

1. Create worker pool
2. Queue compilation tasks
3. Each worker calls `compile/4` or `compile_and_track/4`
4. Collect results
5. Return list of tracked artifacts

**Worker Count**: Configured by `jobs` option, defaults to number of CPUs
