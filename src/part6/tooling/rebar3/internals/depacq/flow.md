# Execution Flow

```mermaid
graph TD
    A[Start: maybe_fetch] --> B{Offline mode?}
    B -->|Yes| C{Dep already exists?}
    B -->|No| D[download_source]

    C -->|Yes| E[Use cached version]
    C -->|No| F[Error: offline, no cache]

    D --> D1[Resource type detection]
    D1 --> D2{Source type?}

    D2 -->|pkg| G[Hex Package Download]
    D2 -->|git| H[Git Clone/Fetch]
    D2 -->|hg| I[Mercurial Clone]

    G --> G1[Check local cache]
    G1 --> G2{Cached with valid ETag?}
    G2 -->|Yes| G3{ETag matches server?}
    G2 -->|No| G4[Download from Hex]

    G3 -->|Yes| G5[Use cached tarball]
    G3 -->|No| G4

    G4 --> G6[r3_hex_repo:get_tarball]
    G6 --> G7[Verify checksum]
    G7 --> G8{Checksum valid?}
    G8 -->|Yes| G9[Store tarball in cache]
    G8 -->|No| G10[Error: bad checksum]

    G9 --> G11[Extract tarball to TmpDir]
    G5 --> G11

    H --> H1{Git version check}
    H1 --> H2{Branch, Tag, or Ref?}
    H2 -->|Branch| H3[git clone -b branch --single-branch]
    H2 -->|Tag| H4[git clone -b tag --single-branch]
    H2 -->|Ref| H5[git clone + checkout ref]

    H3 --> H6[Clone complete]
    H4 --> H6
    H5 --> H6

    I --> I1[hg clone]
    I1 --> I2{Tag or revision?}
    I2 -->|Tag| I3[hg update tag]
    I2 -->|Revision| I4[hg update -r rev]

    I3 --> I5[Clone complete]
    I4 --> I5

    G11 --> J[Move TmpDir to FetchDir]
    H6 --> J
    I5 --> J
    E --> J

    J --> K[Read dep's rebar.config]
    K --> L[Update app info with config]
    L --> M[find_app: Discover application]

    M --> M1{Application found?}
    M1 -->|Yes| N[Mark as available]
    M1 -->|No| O[Error: dep_app_not_found]

    N --> P[Return updated AppInfo]

    style A fill:#e1f5ff
    style P fill:#e1ffe1
    style F fill:#ffe1e1
    style G10 fill:#ffe1e1
    style O fill:#ffe1e1
```

## Detailed Steps

1. **Fetch Decision** (`maybe_fetch/5`)
   - Check if dependency already exists locally
   - Check if update is needed
   - Skip if checkout dependency (already local)
   - Decide whether to fetch or use cached version

2. **Source Type Detection**
   - Determine resource type from source tuple:
     - `{pkg, Name, Version, _, _}` → Hex package
     - `{git, URL, RefSpec}` → Git repository
     - `{hg, URL, RevSpec}` → Mercurial repository

3. **Offline Mode Handling**
   - If offline mode and dependency not cached: error
   - If offline mode and dependency cached: use cache
   - Otherwise proceed with download

4. **Package Download (Hex)** (`rebar_pkg_resource:download/4`)
   - Check local cache directory for existing package
   - Read ETag file to get cached ETag
   - Request package from Hex with If-None-Match header
   - If 304 response: use cached tarball
   - If 200 response: download new tarball
   - Verify checksum against registry
   - Store tarball and ETag in cache
   - Extract tarball to temporary directory

5. **Git Clone** (`rebar_git_resource:download/4`)
   - Detect Git version for optimal clone command
   - For branches: `git clone -b branch --single-branch`
   - For tags: `git clone -b tag --single-branch`
   - For refs: `git clone` + `git checkout ref`
   - Clone to temporary directory
   - Handle authentication if required

6. **Mercurial Clone** (`rebar_hg_resource:download/4`)
   - Use `hg clone` to clone repository
   - Update to specific tag or revision
   - Clone to temporary directory

7. **Extraction and Placement**
   - Create temporary directory with `ec_file:insecure_mkdtemp/0`
   - Download/extract to temporary directory
   - Remove old cached version if exists
   - Move temporary directory to final location
   - Final location: `_build/PROFILE/lib/DEPNAME/`

8. **Post-Download Verification**
   - Read dependency's `rebar.config`
   - Update app info with dependency's configuration
   - Discover application structure with `rebar_app_discover:find_app/4`
   - Verify application is valid
   - Mark application as available
