# Directory Structure

## Project Layout

**Single-App Project**:

```
my_app/
├── rebar.config           % Project configuration
├── rebar.lock             % Locked dependencies
├── src/                   % Source files
│   ├── my_app.app.src    % Application resource
│   └── *.erl             % Erlang modules
├── include/               % Header files
│   └── *.hrl
├── priv/                  % Private resources
├── test/                  % Test files (extra_src_dirs)
└── _build/                % Build output (gitignored)
    └── default/           % Default profile
        ├── lib/           % Dependencies + project
        │   ├── my_app/    % Project output
        │   │   ├── ebin/  % Compiled .beam files
        │   │   ├── priv/  % Copied priv resources
        │   │   └── ...
        │   └── dep_name/  % Each dependency
        ├── extras/        % Extra src_dirs output
        │   └── test/      % Test modules
        └── .rebar3/       % Rebar3 metadata
            └── */         % Compiler DAGs
```

**Umbrella Project**:

```
my_project/
├── rebar.config
├── apps/                  % Multiple applications
│   ├── app1/
│   │   ├── src/
│   │   │   ├── app1.app.src
│   │   │   └── *.erl
│   │   └── include/
│   └── app2/
│       └── src/
└── _build/
    └── default/
        └── lib/
            ├── app1/
            ├── app2/
            └── deps.../
```

## Build Directories

**Base Directory**: `_build/`

- Default location for all build outputs
- Configurable via `base_dir` option or `REBAR_BASE_DIR` env var

**Profile Directories**: `_build/PROFILE/`

- Separate output for each profile (default, test, prod, etc.)
- Isolates artifacts between profiles

**Lib Directory**: `_build/PROFILE/lib/`

- Contains both dependencies and project applications
- Each app in its own subdirectory

**Extras Directory**: `_build/PROFILE/extras/`

- Output for `extra_src_dirs` (test, scripts, etc.)
- Not included in releases

**Metadata Directory**: `_build/PROFILE/.rebar3/`

- Compiler DAG files
- Other rebar3 metadata

## Cache Directories

**Global Cache**: `~/.cache/rebar3/`

- Default location (configurable via `REBAR_CACHE_DIR`)
- Hex packages
- Git repositories

**Structure**:

```
~/.cache/rebar3/
├── hex/
│   └── default/
│       └── packages/
│           ├── package-1.0.0.tar
│           └── package-1.0.0.etag
├── git/
│   └── repo-hash/         % Git clones
└── plugins/               % Global plugins
```

## Configuration Directories

**Global Config**: `~/.config/rebar3/`

```
~/.config/rebar3/
└── rebar.config           % Global configuration
```
