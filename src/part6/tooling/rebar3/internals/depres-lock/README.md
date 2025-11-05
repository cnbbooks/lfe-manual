# Dependency Resolution & Locking

## Purpose

The dependency resolution and locking stage determines which dependencies are required for the project, resolves version constraints for package dependencies, builds a complete dependency tree, detects circular dependencies, and ensures reproducible builds through lock files.

## When It Executes

This stage executes after [Initialization & Configuration](/part6/tooling/rebar3/internals/init-cfg/) and is typically triggered by:

- The `install_deps` provider (dependency of `compile`)
- The `lock` provider (explicitly locking dependencies)
- Any command that requires dependencies to be resolved

The `compile` provider depends on the `lock` provider, which depends on `install_deps`.

## Prerequisites

- State initialized with configuration loaded
- `rebar.config` parsed with dependencies list
- `rebar.lock` read (if exists) with locked versions
- Application discovery completed (for project apps)

## Outputs

- Complete list of all dependencies (direct and transitive)
- Dependency tree with levels (depth from project root)
- Resolved versions for all package dependencies
- Updated `rebar.lock` file with locked versions
- Topologically sorted dependency list (compilation order)
- Updated state with:
  - `all_deps`: All resolved dependencies
  - `deps_to_build`: Dependencies that need compilation
  - `lock`: Current lock data
