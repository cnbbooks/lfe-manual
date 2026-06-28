# Edge Cases

## Cached Package with Modified ETag

**Scenario**: Cached package exists but ETag file doesn't match

**Behavior**:

- Re-download package
- Verify checksum
- Update cache and ETag

---

## Interrupted Download

**Scenario**: Download interrupted mid-process

**Behavior**:

- Temporary directory removed on error
- Next run will re-attempt download
- Cached packages may be incomplete

**Protection**: Always use temporary directory first, then atomic move

---

## Git Shallow Clone Limitations

**Behavior**: rebar3 uses `--single-branch` for optimization

**Limitation**: Cannot switch to other branches without re-cloning

**Impact**: Minimal; dependencies shouldn't change branches

---

## SSH vs HTTPS Git URLs

**SSH**: `git@github.com:user/repo.git`
**HTTPS**: `https://github.com/user/repo.git`

**Differences**:

- SSH requires SSH keys configured
- HTTPS may prompt for credentials
- Both work equivalently for fetching

**Lock File**: Both normalize to same format

---

## Git Tag vs Branch vs Ref

**Tag**: Immutable reference to specific commit
**Branch**: Mutable, points to latest commit on branch
**Ref**: Direct commit SHA

**Recommendation**: Use tags for releases, refs for locking

**Example**:

```erlang
{deps, [
    {cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.9.0"}}}
]}.
```

**Lock File Result**:

```erlang
{<<"cowboy">>, {git, "https://github.com/ninenines/cowboy.git",
                     {ref, "abc123..."}}, 0}
```

Note: Tag converted to ref in lock file

---

## Local Git URLs

**Format**: `file:///path/to/repo.git` or `/path/to/repo`

**Warning**:

```
Local git resources (file:///...) are unsupported and may have odd behaviour.
Use remote git resources, or a plugin for local dependencies.
```

**Recommendation**: Use `_checkouts/` for local development

---

## Package Download with Slow Connection

**Behavior**: May timeout after default httpc timeout

**Solution**: Configure longer timeout in `~/.config/rebar3/rebar.config`:

```erlang
{hex, [
    {http_timeout, 120000}  % 120 seconds
]}.
```

---

## Hex CDN Changes

**Multiple CDNs Supported**:

- Official: `https://repo.hex.pm`
- Mirrors: Various regions

**Configuration**: Use `HEX_CDN` environment variable

**Example**:

```bash
HEX_CDN=https://hexpm.example.com rebar3 compile
```

---

## Transitive Dependencies of Git Deps

**Scenario**: Git dependency has its own dependencies

**Behavior**:

- After cloning Git dep, read its `rebar.config`
- Resolve its dependencies recursively
- Add to dependency tree

**Example**:

```
my_app → git_dep (Git)
git_dep → hex_dep (Hex)
```

Both fetched, Git first, then Hex
