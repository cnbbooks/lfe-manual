# Configuration

## Resource Configuration

### Hex Repository Configuration

**In `rebar.config`**:

```erlang
{hex, [
    {repos, [
        #{name => <<"hexpm">>,
          url => <<"https://repo.hex.pm">>,
          auth_key => {<<"SOME_KEY">>}
        }
    ]}
]}.
```

**Default Hex CDN**: `https://repo.hex.pm`

**Environment Variables**:

- `HEX_CDN`: Override Hex CDN URL
- `HEX_MIRROR`: Alternative to HEX_CDN (for Mix compatibility)

### Git Configuration

**Authentication**:

- SSH: Uses SSH keys from `~/.ssh/`
- HTTPS: May prompt for credentials (disabled with `GIT_TERMINAL_PROMPT=0`)
- Token: Can embed in URL: `https://token@github.com/user/repo.git`

**Git Version Requirements**:

- Minimum: 1.8.5 (for locking with `-C` flag)
- Recommended: 2.3.0+ (for `--single-branch` optimization)

## Cache Directories

**Package Cache**: `~/.cache/rebar3/hex/REPONAME/packages/`

**Contents**:

- `PACKAGE-VERSION.tar`: Package tarball
- `PACKAGE-VERSION.etag`: ETag for cache validation

**Environment Variable**: `REBAR_CACHE_DIR` overrides cache location
