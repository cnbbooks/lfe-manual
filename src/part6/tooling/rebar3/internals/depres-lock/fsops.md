# File System Operations

## Files Read

| File | Purpose | When | Required |
|------|---------|------|----------|
| `rebar.lock` | Get locked versions | Start of stage | No |
| `_checkouts/*/rebar.config` | Checkout dep configs | During traversal | If checkout exists |
| `_build/*/lib/*/rebar.config` | Cached dep configs | During traversal | For already-fetched deps |

## Files Written

| File | Content | When | Conditions |
|------|---------|------|------------|
| `rebar.lock` | Locked dependency versions | End of lock provider | Locks changed AND default profile |

## Directories Accessed

- `_checkouts/`: Checkout dependencies
- `_build/PROFILE/lib/`: Dependency locations
- Various remote locations for fetching (if needed)
