# State Modification

## No Direct State Modifications

This stage doesn't modify the global state directly. Instead, it:

- Downloads files to filesystem
- Updates `rebar_app_info:t()` records
- These updated records are accumulated in the resolution stage

## AppInfo Modifications

For each fetched dependency, the `rebar_app_info:t()` record is updated:

| Field | Update | Value |
|-------|--------|-------|
| `dir` | Set | Directory where dep was extracted |
| `fetch_dir` | Set | Same as `dir` |
| `opts` | Merged | With dependency's `rebar.config` |
| `is_available` | Set | `true` after successful fetch |
| `valid` | Set | `true` after app discovery |
