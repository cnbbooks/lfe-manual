# File System Operations

## Files Read

| File | When | Purpose |
|------|------|---------|
| `_build/PROFILE/.rebar3/COMPILER/source*.dag` | `init/4` | Restore cached DAG |
| Source files | `populate_sources/5` | Check timestamps |
| Header files | `populate_deps/3` | Check timestamps |

## Files Written

| File | When | Content |
|------|------|---------|
| `_build/PROFILE/.rebar3/COMPILER/source*.dag` | `maybe_store/5` | Serialized DAG |

## Directories Created

| Directory | When |
|-----------|------|
| `_build/PROFILE/.rebar3/COMPILER/` | Before saving DAG |
