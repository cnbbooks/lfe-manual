# Dependency Graph Management

## Purpose

The Dependency Graph (DAG) management system tracks relationships between source files, header files, and compiled artifacts. It enables incremental compilation by maintaining timestamps, detecting changes, propagating modifications through dependency chains, determining inter-application compilation order, and persisting graph data between builds for performance.

## When It Executes

DAG operations occur throughout the compilation process within [Source File Compilation](/part6/tooling/rebar3/internals/srcflcomp):

1. **Initialization**: `load_dags/2` - Load or create DAGs before compilation
2. **Population**: `populate_sources/5` - Add source files and scan dependencies
3. **Pruning**: `prune/5` - Remove deleted files
4. **Dependency Scanning**: `populate_deps/3` - Scan header files
5. **Timestamp Propagation**: `propagate_stamps/1` - Update timestamps through chains
6. **Compilation Ordering**: `compile_order/4` - Determine inter-app order
7. **Artifact Tracking**: `store_artifact/4` - Record compiled outputs
8. **Persistence**: `maybe_store/5` - Save DAG if modified

## Prerequisites

- Output directory exists for DAG storage
- Compiler modules loaded
- Applications discovered with source files

## Outputs

- In-memory directed graphs tracking dependencies
- Persisted DAG files in `_build/PROFILE/.rebar3/COMPILER/source[_LABEL].dag`
- Compilation order based on dependencies
- List of files needing recompilation
