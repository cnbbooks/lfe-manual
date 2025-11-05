# State Modification

## Fields Modified During This Stage

| Field | Operation | Value | Purpose |
|-------|-----------|-------|---------|
| `all_deps` | Set | All resolved dependencies | Complete dep list |
| `deps_to_build` | Set | Filtered dependency list | Deps needing compilation |
| `lock` | Updated | Lock entries | For writing to `rebar.lock` |
| `{locks, default}` | Set | Saved lock data | Lock file contents |
| `code_paths` (all_deps key) | Set | Dep ebin directories | For compilation phase |

## Dependency Information Stored

Each dependency (`rebar_app_info:t()`) contains:

- `name`: Dependency name
- `source`: Where it comes from (Hex, Git, etc.)
- `dep_level`: Depth in dependency tree (0 = direct)
- `dir`: Source directory
- `out_dir`: Build output directory
- `is_checkout`: Whether it's a checkout dependency
- `is_lock`: Whether from lock file
- `deps`: Transitive dependencies
