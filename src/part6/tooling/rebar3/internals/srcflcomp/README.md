# Source File Compilation

## Purpose

The source file compilation stage is the core of the build process. It compiles source files (`.erl`, `.yrl`, `.xrl`, `.mib`, etc.) into their target artifacts (`.beam`, `.bin`, `.hrl`), managing dependencies between files, determining what needs recompilation, executing compilers in the correct order, supporting parallel compilation where possible, and tracking compiled artifacts for incremental builds.

## When It Executes

This stage executes within `rebar_prv_compile` after application discovery and ordering:

1. **Dependency Compilation**: `copy_and_build_deps/4` compiles all dependencies
2. **Project App Compilation**: `copy_and_build_project_apps/3` compiles project applications
3. **Root Extras Compilation**: `build_root_extras/2` compiles extra source directories

Within each context, the `compile/4` function orchestrates multiple sub-stages.

## Prerequisites

- Applications discovered and ordered
- Dependencies fetched and ready
- Output directories created
- Code paths configured
- DAGs (Dependency Graphs) initialized for each compiler

## Outputs

- Compiled `.beam` files in `ebin/` directories
- Generated header files (`.hrl` from `.yrl`, `.mib`)
- Generated source files (`.erl` from `.yrl`, `.xrl`)
- Updated DAGs with artifact metadata and timestamps
- Code available for subsequent compilation stages
