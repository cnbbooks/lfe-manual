# State Modification

## Fields Modified

| Field | Operation | Value | Purpose |
|-------|-----------|-------|---------|
| `project_apps` | Set | List of `rebar_app_info:t()` | Discovered project applications |
| `{parsed_deps, Profile}` | Set per profile | List of parsed deps | Dependencies per profile |
| `{deps, Profile}` | Updated per profile | Merged dep list | Combined top-level and app deps |

## App Info Fields Set

| Field | Value | Purpose |
|-------|-------|---------|
| `name` | Application name (binary) | Application identifier |
| `dir` | Application source directory | Where source files are |
| `out_dir` | Build output directory | Where compiled files go |
| `ebin_dir` | `out_dir/ebin` | Where .beam files go |
| `opts` | Merged configuration | App-specific config |
| `app_details` | Parsed .app data | Application metadata |
| `applications` | Runtime dependencies | From .app file |
| `deps` | Build dependencies | From rebar.config |
