# Directory Configuration

## lib_dirs

**Purpose**: Directories to search for applications

**Type**: `[Dir :: string()]`

**Default**: `[]`

**Example**:

```erlang
{lib_dirs, ["apps", "components"]}.
```

## base_dir

**Purpose**: Build output directory

**Type**: `string()`

**Default**: `"_build"`

**Environment**: `REBAR_BASE_DIR`

**Example**:

```erlang
{base_dir, "build"}.
```

## deps_dir

**Purpose**: Where to place dependencies

**Type**: `string()`

**Default**: `"lib"` (within profile directory)

**Example**:

```erlang
{deps_dir, "external"}.
% Results in: _build/default/external/
```
