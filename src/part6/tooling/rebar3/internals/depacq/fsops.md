# File System Operations

## Files Read

| File | Purpose | When | Required |
|------|---------|------|----------|
| `CACHE/name-version.tar` | Cached package | Package download | No |
| `CACHE/name-version.etag` | Cache validation | Package download | No |
| `DEP/rebar.config` | Dep configuration | After download | Yes |
| `.git/config` | Git remote URL | needs_update check | For Git deps |

## Files Written

| File | Content | When | Location |
|------|---------|------|----------|
| Package tarball | Hex package | First download | `~/.cache/rebar3/hex/.../name-version.tar` |
| ETag file | HTTP ETag | After download | `~/.cache/rebar3/hex/.../name-version.etag` |
| Dependency files | Source code | After download | `_build/PROFILE/lib/DEPNAME/` |

## Directories Created

| Directory | Purpose | When |
|-----------|---------|------|
| `_build/PROFILE/lib/` | Dependency storage | Before first dep download |
| `_build/PROFILE/lib/DEPNAME/` | Individual dependency | Per dependency |
| Temporary directory | Staging area | During download |
| `~/.cache/rebar3/hex/REPO/packages/` | Package cache | First Hex download |

## Directories Removed

| Directory | When | Why |
|-----------|------|-----|
| Old dep version | Before moving new version | Clean old version |
| Temporary directory | After successful move | Cleanup |
| Failed download temp | On error | Cleanup |
