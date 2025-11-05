# Example Scenarios

## Scenario 1: First Hex Package Download

**Dependency**: `{jsx, "3.1.0"}`

**Execution**: `rebar3 compile` (first time)

**Flow**:

1. Resolution determines need for jsx 3.1.0
2. Call `maybe_fetch` → needs download
3. Call `rebar_pkg_resource:download/4`
4. Check cache: `~/.cache/rebar3/hex/hexpm/packages/jsx-3.1.0.tar` doesn't exist
5. Request from Hex: `GET /tarballs/jsx-3.1.0`
6. Download tarball (HTTP 200)
7. Verify checksum: `sha256(tarball)` vs registry checksum
8. Store in cache with ETag
9. Extract to `_build/default/lib/jsx/`
10. Read `jsx/rebar.config`
11. Discover jsx application
12. Mark as available

**Files Created**:

- `~/.cache/rebar3/hex/hexpm/packages/jsx-3.1.0.tar`
- `~/.cache/rebar3/hex/hexpm/packages/jsx-3.1.0.etag`
- `_build/default/lib/jsx/` (full source tree)

---

## Scenario 2: Cached Hex Package (ETag Valid)

**Dependency**: `{jsx, "3.1.0"}`

**Execution**: `rebar3 compile` (after clean, cache still valid)

**Flow**:

1. Check cache: tarball exists
2. Read ETag file: `"abc123"`
3. Request from Hex with `If-None-Match: "abc123"`
4. Hex returns HTTP 304 Not Modified
5. Use cached tarball
6. Extract to `_build/default/lib/jsx/`
7. Discover application

**Network**: Minimal (only HTTP HEAD request equivalent)

---

## Scenario 3: Git Dependency with Tag

**Dependency**:

```erlang
{cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.9.0"}}}
```

**Execution**: `rebar3 compile` (first time)

**Flow**:

1. Call `rebar_git_resource:download/4`
2. Detect Git version: 2.30.0
3. Execute:

   ```bash
   git clone --depth 1 --no-single-branch \
     https://github.com/ninenines/cowboy.git \
     /tmp/rebar-abc123/cowboy \
     -b 2.9.0 --single-branch
   ```

4. Clone completes to temporary directory
5. Move to `_build/default/lib/cowboy/`
6. Read `cowboy/rebar.config`:

   ```erlang
   {deps, [cowlib, ranch]}.
   ```

7. Discover cowboy application
8. Add cowlib and ranch to dependency queue

**Subsequent Runs**:

- Check if update needed: `git describe --tags --exact-match`
- Compare with "2.9.0"
- If match: skip re-download

---

## Scenario 4: Git Dependency with Branch

**Dependency**:

```erlang
{my_dep, {git, "https://github.com/user/my_dep.git", {branch, "main"}}}
```

**First Run**:

1. Clone with `-b main --single-branch`
2. Extract to `_build/default/lib/my_dep/`

**Subsequent Runs**:

1. Check for updates:

   ```bash
   cd _build/default/lib/my_dep
   git fetch origin main
   git log HEAD..origin/main --oneline
   ```

2. If output not empty: new commits available
3. Pull updates
4. Re-compile

**Warning**: Branches are mutable; can cause non-reproducible builds

---

## Scenario 5: Offline Mode with Cache

**Setup**:

- jsx 3.1.0 previously downloaded and cached
- Offline mode enabled

**Execution**: `REBAR_OFFLINE=1 rebar3 compile`

**Flow**:

1. Attempt fetch for jsx
2. Detect offline mode
3. Check cache: tarball exists
4. Use cached tarball without network request
5. Extract and proceed

**Success**: Works without network

---

## Scenario 6: Offline Mode Without Cache

**Setup**:

- Fresh dependency not in cache
- Offline mode enabled

**Execution**: `REBAR_OFFLINE=1 rebar3 compile`

**Flow**:

1. Attempt fetch for new_dep
2. Detect offline mode
3. Check cache: not found
4. Error: cannot fetch in offline mode

**Error**:

```
Cannot fetch dependency in offline mode
```

**Solution**: Disable offline mode or pre-cache

---

## Scenario 7: Corrupted Package Cache

**Setup**: Cached package has wrong checksum (corruption or tampering)

**Execution**: `rebar3 compile`

**Flow**:

1. Check cache: jsx-3.1.0.tar exists
2. Use cached tarball
3. Calculate checksum
4. Compare with registry
5. **Mismatch detected**
6. Clear ETag file
7. Error reported

**Error**:

```
The checksum for package at jsx-3.1.0 (WRONG_HASH) does not match
the checksum expected from the registry (CORRECT_HASH).
Run `rebar3 do unlock jsx, update` and then try again.
```

**Recovery**:

```bash
rm ~/.cache/rebar3/hex/hexpm/packages/jsx-3.1.0.*
rebar3 compile
```

---

## Scenario 8: Authentication Required for Private Git Repo

**Dependency**:

```erlang
{private_dep, {git, "git@github.com:company/private_dep.git", {tag, "1.0.0"}}}
```

**Execution**: `rebar3 compile`

**Prerequisites**: SSH key added to GitHub account

**Flow**:

1. Git uses SSH authentication
2. Reads `~/.ssh/id_rsa` or configured key
3. Authenticates with GitHub
4. Clone proceeds normally

**If Authentication Fails**:

```
Permission denied (publickey).
fatal: Could not read from remote repository.
```

**Solution**:

- Add SSH key to GitHub
- Or use HTTPS with token: `https://TOKEN@github.com/company/private_dep.git`
