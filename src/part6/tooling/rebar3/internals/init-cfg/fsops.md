# File System Operations

## Files Read

| File | Purpose | Required | When Read |
|------|---------|----------|-----------|
| `rebar.config` | Project configuration | No (uses defaults) | `init_config/0` |
| `rebar.lock` | Locked dependencies | No | `init_config/0` |
| `~/.config/rebar3/rebar.config` | Global configuration | No | `init_config/0` |
| Escript archive | Plugin/template extraction | No | When needed |

## Files Written

None in this stage. Files are written in later stages.

## Directories Created

None in this stage. Directories are created during compilation.

## Directories Accessed

- Current working directory (project root)
- `~/.config/rebar3/` (for global config)
- `~/.cache/rebar3/` (may be accessed for cached data)
