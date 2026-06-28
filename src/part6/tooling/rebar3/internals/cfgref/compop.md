# Compilation Options

## erl_opts

**Purpose**: Erlang compiler options

**Type**: `[term()]`

**Common Options**:

```erlang
{erl_opts, [
    debug_info,                     % Include debug information
    warnings_as_errors,             % Treat warnings as errors
    warn_export_vars,               % Warn about exported variables
    warn_unused_import,             % Warn about unused imports
    warn_missing_spec,              % Warn about missing specs
    {parse_transform, Module},      % Apply parse transform
    {d, 'MACRO'},                   % Define macro
    {d, 'MACRO', Value},            % Define macro with value
    {i, "include"},                 % Include directory
    {i, "/abs/path/include"},       % Absolute include path
    no_debug_info,                  % Exclude debug information
    inline_list_funcs,              % Inline list functions
    {platform_define, Regex, Def},  % Platform-specific defines
    {src_dirs, ["src", "lib"]},     % Source directories
    compressed,                     % Compress beam files
    warn_export_all,                % Warn on export_all
    warn_shadow_vars,               % Warn about shadowed variables
    warn_obsolete_guard,            % Warn about obsolete guards
    warn_unused_record,             % Warn about unused records
    warn_unused_vars,               % Warn about unused variables
    nowarn_deprecated_function,     % Don't warn on deprecated functions
    {nowarn_unused_function, [{F,A}]}, % Specific functions
    {nowarn_unused_type, [{T,A}]}   % Specific types
]}.
```

**Warning Control**:

```erlang
{erl_opts, [
    {warn_format, 2},               % Warning level for format strings
    {error_location, column},       % Include column in errors
    {feature, maybe_expr, enable}   % Enable experimental features
]}.
```

## src_dirs

**Purpose**: Source directories to search for `.erl` files

**Type**: `[Dir :: string()]` or `[{Dir, Opts}]`

**Default**: `["src"]`

**Examples**:

```erlang
{src_dirs, ["src", "lib", "core"]}.

{src_dirs, [
    "src",
    {"lib", [{recursive, true}]}
]}.
```

## extra_src_dirs

**Purpose**: Additional source directories (compiled separately, not in .app modules list)

**Type**: `[Dir :: string()]`

**Default**: `[]`

**Common Use**: Test, script, example directories

**Example**:

```erlang
{extra_src_dirs, ["test", "scripts"]}.
```

## erl_first_files

**Purpose**: Files to compile before others

**Type**: `[File :: string()]`

**Use Cases**: Parse transforms, macros, type definitions

**Example**:

```erlang
{erl_opts, [
    {erl_first_files, ["src/my_transform.erl", "src/types.erl"]}
]}.
```

## validate_app_modules

**Purpose**: Validate all modules in `.app` exist as `.beam` files

**Type**: `boolean()`

**Default**: `true`

**Example**:

```erlang
{validate_app_modules, false}.
```
