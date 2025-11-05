# File System Operations

## Files Read

| File | Purpose | Required |
|------|---------|----------|
| `APP/src/*.app.src` | Application resource | At least one |
| `APP/ebin/*.app` | Compiled app resource | Alternative to .app.src |
| `APP/src/*.app.src.script` | Dynamic app resource | Alternative |
| `APP/mix.exs` | Elixir Mix project | Alternative |
| `APP/rebar.config` | App-specific config | No |

## Directories Scanned

| Directory Pattern | Purpose |
|-------------------|---------|
| `lib_dirs/*` | Find application directories |
| `*/src/`, `*/lib/` | Search for app resources |
| `*/ebin/` | Search for compiled apps |
