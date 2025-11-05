# Functions & API Calls

## `maybe_fetch/5`

**Purpose**: Determine if dependency needs fetching and fetch if necessary

**Signature**:

```erlang
-spec maybe_fetch(AppInfo, Profile, Upgrade, Seen, State) -> {Cached | false, AppInfo} when
    AppInfo :: rebar_app_info:t(),
    Profile :: atom(),
    Upgrade :: boolean(),
    Seen :: sets:set(),
    State :: rebar_state:t(),
    Cached :: boolean().
```

**Arguments**:

- `AppInfo`: Dependency application info
- `Profile`: Current profile
- `Upgrade`: Whether in upgrade mode
- `Seen`: Set of already-seen dependencies
- `State`: Current rebar state

**Returns**: `{Cached, UpdatedAppInfo}`

**Flow**:

1. Check if checkout dependency → skip fetch
2. Check if already exists locally
3. If exists and not upgrade mode → check if needs update
4. If needs fetch/update → call `rebar_fetch:download_source/2`
5. Return updated app info

---

## `rebar_fetch:download_source/2`

**Purpose**: Main entry point for downloading any dependency

**Signature**:

```erlang
-spec download_source(rebar_app_info:t(), rebar_state:t()) ->
    rebar_app_info:t() | {error, any()}.
```

**Arguments**:

- `AppInfo` (`rebar_app_info:t()`): Dependency to download
- `State` (`rebar_state:t()`): Current state

**Returns**:

- Updated `AppInfo` with dependency downloaded
- `{error, Reason}` if download failed

**Flow**:

1. Check offline mode → error if offline and not cached
2. Call `download_source_online/2`
3. After download: read dependency's `rebar.config`
4. Update app info opts with dependency's config
5. Discover application with `rebar_app_discover:find_app/4`
6. Mark as available
7. Return updated app info

**Example Usage**:

```erlang
AppInfo1 = rebar_fetch:download_source(AppInfo, State)
```

---

## `download_source_online/2`

**Purpose**: Perform actual download operation

**Signature**:

```erlang
-spec download_source_online(rebar_app_info:t(), rebar_state:t()) -> ok | {error, term()}.
```

**Arguments**:

- `AppInfo`: Dependency to download
- `State`: Current state

**Returns**: `ok` or `{error, Reason}`

**Flow**:

1. Get app directory from app info
2. Create temporary directory with `ec_file:insecure_mkdtemp/0`
3. Call `rebar_resource_v2:download/3` with TmpDir
4. Resource module downloads to TmpDir
5. Ensure app directory exists
6. Remove old version from code path
7. Remove old fetch directory
8. Move TmpDir to final location (FetchDir)

---

## `rebar_resource_v2:download/3`

**Purpose**: Dispatch to appropriate resource module for download

**Signature**:

```erlang
-spec download(TmpDir, AppInfo, State) -> ok | {error, term()} when
    TmpDir :: file:filename(),
    AppInfo :: rebar_app_info:t(),
    State :: rebar_state:t().
```

**Arguments**:

- `TmpDir`: Temporary directory for download
- `AppInfo`: Dependency info with source
- `State`: Current state

**Returns**: `ok` or `{error, Reason}`

**Flow**:

- Determine resource type from source
- Call appropriate resource module's `download/4`:
  - `rebar_pkg_resource:download/4` for Hex packages
  - `rebar_git_resource:download/4` for Git repos
  - `rebar_hg_resource:download/4` for Mercurial repos

---

## `rebar_pkg_resource:download/4`

**Purpose**: Download Hex package

**Signature**:

```erlang
-spec download(TmpDir, AppInfo, State, ResourceState) -> ok | {error, term()} when
    TmpDir :: file:name(),
    AppInfo :: rebar_app_info:t(),
    State :: rebar_state:t(),
    ResourceState :: rebar_resource_v2:resource_state().
```

**Arguments**:

- `TmpDir`: Temporary directory
- `AppInfo`: Package info
- `State`: Current state
- `ResourceState`: Resource-specific state (repos config)

**Returns**: `ok` or `{error, Reason}`

**Flow**:

1. Get package cache directory
2. Build cache path: `~/.cache/rebar3/hex/default/packages/name-version.tar`
3. Build ETag file path: `~/.cache/rebar3/hex/default/packages/name-version.etag`
4. Call `cached_download/7` with cache info
5. Extract tarball contents to TmpDir

---

## `cached_download/7`

**Purpose**: Download package with caching and ETag support

**Signature**:

```erlang
-spec cached_download(TmpDir, CachePath, Pkg, State, ETag, ETagPath, UpdateETag) ->
    ok | {error, term()} when
    TmpDir :: file:name(),
    CachePath :: file:name(),
    Pkg :: package(),
    State :: rebar_state:t(),
    ETag :: binary() | undefined,
    ETagPath :: file:name(),
    UpdateETag :: boolean().
```

**Flow**:

1. Check if cached tarball exists
2. If exists: read ETag from ETag file
3. Request package from Hex with ETag (If-None-Match header)
4. If 304 Not Modified: use cached tarball
5. If 200 OK: download new tarball
6. Verify checksum: `calc_checksum(Tarball)` vs registry checksum
7. If valid: store tarball and ETag in cache
8. Extract tarball to TmpDir

---

## `r3_hex_repo:get_tarball/3`

**Purpose**: HTTP request to Hex for package tarball

**Signature**:

```erlang
-spec get_tarball(Config, Name, Version) ->
    {ok, {StatusCode, Headers, Body}} | {error, Reason} when
    Config :: map(),
    Name :: binary(),
    Version :: binary(),
    StatusCode :: integer(),
    Headers :: map(),
    Body :: binary(),
    Reason :: term().
```

**Arguments**:

- `Config`: Hex repo configuration (includes ETag)
- `Name`: Package name
- `Version`: Package version

**Returns**:

- `{ok, {200, Headers, Tarball}}`: New tarball downloaded
- `{ok, {304, Headers, _}}`: Cached version still valid
- `{ok, {Code, _, _}}`: Other HTTP status
- `{error, Reason}`: Network/HTTP error

**Headers**:

- `<<"etag">>`: ETag value for caching

---

## `rebar_git_resource:download/4`

**Purpose**: Clone Git repository

**Signature**:

```erlang
-spec download(TmpDir, AppInfo, State, ResourceState) -> ok | {error, term()} when
    TmpDir :: file:name(),
    AppInfo :: rebar_app_info:t(),
    State :: rebar_state:t(),
    ResourceState :: rebar_resource_v2:resource_state().
```

**Arguments**:

- `TmpDir`: Temporary directory for clone
- `AppInfo`: Git source info
- `State`: Current state
- `ResourceState`: Resource state

**Returns**: `ok` or `{error, Reason}`

**Flow**:

1. Ensure TmpDir exists
2. Detect Git version with `git_vsn/0`
3. Parse source ref spec (branch/tag/ref)
4. Call appropriate `git_clone/5` variant
5. Clone to `TmpDir/basename`

---

## `git_clone/5`

**Purpose**: Execute git clone with appropriate options

**Signature**:

```erlang
-spec git_clone(Type, GitVersion, Url, Dir, RefSpec) -> ok | {error, term()} when
    Type :: branch | tag | ref | rev,
    GitVersion :: {Major, Minor, Patch} | undefined,
    Url :: string(),
    Dir :: file:name(),
    RefSpec :: string().
```

**Arguments**:

- `Type`: Type of ref (branch, tag, ref, rev)
- `GitVersion`: Detected Git version
- `Url`: Git repository URL
- `Dir`: Target directory
- `RefSpec`: Specific branch/tag/ref value

**Commands Based on Git Version**:

**Branch (Git >= 2.3.0)**:

```bash
git clone [options] URL DIR -b BRANCH --single-branch
```

**Branch (Git < 2.3.0)**:

```bash
git clone [options] URL DIR
cd DIR && git checkout -b BRANCH origin/BRANCH
```

**Tag (Git >= 2.3.0)**:

```bash
git clone [options] URL DIR -b TAG --single-branch
```

**Tag (Git < 2.3.0)**:

```bash
git clone [options] URL DIR
cd DIR && git checkout TAG
```

**Ref**:

```bash
git clone [options] URL DIR
cd DIR && git checkout REF
```

**Environment**:

- `GIT_TERMINAL_PROMPT=0`: Disable interactive prompts

---

## `rebar_git_resource:lock/2`

**Purpose**: Get current commit ref for locking

**Signature**:

```erlang
-spec lock(AppInfo, ResourceState) -> {git, Url, {ref, Ref}} when
    AppInfo :: rebar_app_info:t(),
    ResourceState :: rebar_resource_v2:resource_state(),
    Url :: string(),
    Ref :: string().
```

**Arguments**:

- `AppInfo`: Dependency app info
- `ResourceState`: Resource state

**Returns**: Lock source tuple with commit ref

**Command**:

```bash
git -C DIR rev-parse --verify HEAD
```

**Result**: Full commit SHA (40 characters)

---

## `rebar_git_resource:needs_update/2`

**Purpose**: Check if Git dependency needs updating

**Signature**:

```erlang
-spec needs_update(AppInfo, ResourceState) -> boolean() when
    AppInfo :: rebar_app_info:t(),
    ResourceState :: rebar_resource_v2:resource_state().
```

**Arguments**:

- `AppInfo`: Dependency info
- `ResourceState`: Resource state

**Returns**: `true` if update needed, `false` otherwise

**Logic by Type**:

**Tag**:

```bash
git describe --tags --exact-match
```

Compare with specified tag and URL

**Branch**:

```bash
git fetch origin BRANCH
git log HEAD..origin/BRANCH --oneline
```

Update needed if new commits exist

**Ref**:

```bash
git rev-parse --short=7 -q HEAD
```

Compare with specified ref (truncated to same length)

---

## `rebar_hg_resource:download/4`

**Purpose**: Clone Mercurial repository

**Signature**:

```erlang
-spec download(TmpDir, AppInfo, State, ResourceState) -> ok | {error, term()} when
    TmpDir :: file:name(),
    AppInfo :: rebar_app_info:t(),
    State :: rebar_state:t(),
    ResourceState :: rebar_resource_v2:resource_state().
```

**Commands**:

```bash
hg clone URL DIR
cd DIR && hg update -r REVISION
```

Or for tags:

```bash
hg clone URL DIR
cd DIR && hg update TAG
```
