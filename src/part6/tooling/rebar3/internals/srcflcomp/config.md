# Configuration

## Erlang Compiler Options

**Configuration Key**: `erl_opts`

**Location**: `rebar.config` or app-specific config

**Common Options**:

```erlang
{erl_opts, [
    debug_info,                    % Include debug information
    warnings_as_errors,            % Treat warnings as errors
    {parse_transform, Module},     % Apply parse transform
    {d, 'MACRO'},                  % Define macro
    {d, 'MACRO', Value},           % Define macro with value
    {i, "include"},                % Include directory
    {platform_define, Regex, Def}, % Platform-specific defines
    {src_dirs, ["src", "lib"]},    % Additional source directories
    no_debug_info,                 % Exclude debug information
    inline_list_funcs             % Inline list functions
]}.
```

## First Files

**Configuration Key**: `erl_first_files`

**Purpose**: Compile specific files before others

**Example**:

```erlang
{erl_opts, [
    {erl_first_files, ["src/parser.erl", "src/types.erl"]}
]}.
```

**Use Cases**:

- Parse transforms (must compile before files using them)
- Type definitions (for other modules)
- Macros (defined in one file, used in others)

## Extra Source Directories

**Configuration Key**: `extra_src_dirs`

**Purpose**: Additional source directories with separate compilation

**Example**:

```erlang
{extra_src_dirs, [
    "test",
    {"scripts", [{recursive, true}]}
]}.
```

**Behavior**:

- Compiled to `_build/PROFILE/extras/DIR/`
- Not included in application's `.app` file modules list
- Use for tests, scripts, examples

## Parallel Compilation

**Configuration Key**: `jobs`

**Default**: Number of logical CPU cores

**Example**:

```erlang
{jobs, 4}.  % Use 4 parallel workers
```
