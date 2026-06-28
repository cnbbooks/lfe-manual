# File System Operations

## Files Read

| File Pattern | Purpose | When |
|--------------|---------|------|
| `src/**/*.erl` | Source files | Compilation |
| `src/**/*.yrl` | Parser specs | Before `.erl` compilation |
| `src/**/*.xrl` | Lexer specs | Before `.erl` compilation |
| `src/**/*.mib` | MIB files | SNMP compilation |
| `include/**/*.hrl` | Header files | Dependency scanning |
| DAG files | Cached dependencies | Start of compilation |

## Files Written

| File Pattern | Source | Compiler |
|--------------|--------|----------|
| `ebin/*.beam` | `.erl` | `rebar_compiler_erl` |
| `src/*.erl` | `.yrl` | `rebar_compiler_yrl` |
| `src/*.erl` | `.xrl` | `rebar_compiler_xrl` |
| `priv/mibs/*.bin` | `.mib` | `rebar_compiler_mib` |
| `include/*.hrl` | `.mib` | `rebar_compiler_mib` |
| `.rebar3/COMPILER/*.dag` | - | All compilers (DAG cache) |

## Directories Created

| Directory | Purpose |
|-----------|---------|
| `_build/PROFILE/lib/APP/ebin/` | Compiled `.beam` files |
| `_build/PROFILE/lib/APP/priv/` | Private resources |
| `_build/PROFILE/extras/DIR/` | Extra source dirs output |
| `_build/PROFILE/.rebar3/COMPILER/` | DAG cache storage |
