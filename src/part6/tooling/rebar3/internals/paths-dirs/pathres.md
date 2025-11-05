# Path Resolution

## Source Directories

**Default Source Directories**:

```erlang
{src_dirs, ["src"]}.
```

**Multiple Source Directories**:

```erlang
{src_dirs, ["src", "lib", "core"]}.
```

**Recursive Source Directories**:

```erlang
{src_dirs, [
    "src",
    {"lib", [{recursive, true}]}
]}.
```

## Include Directories

**Include Path Priority** (for `rebar_compiler_erl`):

1. Application's `include/` directory
2. Directories from `{i, Dir}` in `erl_opts`
3. All source directories (recursively if configured)
4. Top-level application directory
5. All project apps' include directories (during compilation)

**Example**:

```erlang
{erl_opts, [
    {i, "custom_include"},
    {i, "/absolute/path/include"}
]}.
```

## Output Directories

**Application Output**: `_build/PROFILE/lib/APPNAME/`

- Mirrors source structure
- `ebin/`: Compiled artifacts
- `priv/`: Copied private resources
- `include/`: Available to other apps

**Dependency Output**: `_build/PROFILE/lib/DEPNAME/`

- Same structure as application output
- Fetched from cache or remote

## Library Directories

**lib_dirs Configuration**:

```erlang
{lib_dirs, ["apps", "components"]}.
```

**Purpose**: Where to find application directories during discovery

**Default**: `[]` (only scan project root)

## Extra Source Directories

**Configuration**:

```erlang
{extra_src_dirs, ["test", "scripts"]}.
```

**Output**: `_build/PROFILE/extras/DIRNAME/`

**Module List**: Not included in `.app` file
