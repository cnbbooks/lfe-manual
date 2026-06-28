# File System Operations

## Files Read

| File | Purpose | Required |
|------|---------|----------|
| `src/APP.app.src` | Application specification | Yes (or .script) |
| `src/APP.app.src.script` | Dynamic specification | Alternative |
| `config/app.vars` | Variable substitution | No |
| `ebin/*.beam` | Module list generation | Yes |

## Files Written

| File | Content | When |
|------|---------|------|
| `ebin/APP.app` | Application resource | Always (if changed) |

## Beam Inspection

Uses `beam_lib:chunks/2` to read:

- `compile_info`: Find source file path
- Used to filter extra directory modules
