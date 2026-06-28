# Advanced Configuration

## minimum_otp_vsn

**Purpose**: Minimum OTP version required

**Type**: `string()`

**Example**:

```erlang
{minimum_otp_vsn, "24"}.
```

**Also Supports Regex**:

```erlang
{minimum_otp_vsn, "24|25"}.
```

## artifacts

**Purpose**: Additional artifacts to verify after compilation

**Type**: `[File :: string()]`

**Variables**: `{{profile}}`, `{{priv_dir}}`

**Example**:

```erlang
{artifacts, [
    "priv/my_nif.so",
    "priv/{{profile}}/custom_artifact"
]}.
```

## project_plugins

**Purpose**: Plugins for this project only (not global)

**Type**: Same as `deps`

**Example**:

```erlang
{project_plugins, [
    rebar3_hex,
    {rebar3_custom, {git, "...", {tag, "1.0.0"}}}
]}.
```

## plugins

**Purpose**: Global plugins

**Type**: Same as `deps`

**Example**:

```erlang
{plugins, [
    rebar3_proper,
    rebar3_ex_doc
]}.
```

## app_vars_file

**Purpose**: External file for `.app.src` variable substitution

**Type**: `string()`

**Example**:

```erlang
{app_vars_file, "config/app.vars"}.
```

**File Contents** (`config/app.vars`):

```erlang
{copyright, "Copyright (c) 2024 Company"}.
{build_date, "2024-01-15"}.
```

## jobs

**Purpose**: Number of parallel compilation workers

**Type**: `pos_integer()`

**Default**: Number of CPU cores

**Example**:

```erlang
{jobs, 4}.
```

## xrl_opts / yrl_opts

**Purpose**: Leex/Yecc compiler options

**Type**: `[term()]`

**Example**:

```erlang
{xrl_opts, [{includefile, "custom.hrl"}]}.
{yrl_opts, [{includefile, "parser.hrl"}]}.
```

## mib_opts

**Purpose**: SNMP MIB compiler options

**Type**: `[term()]`

**Example**:

```erlang
{mib_opts, [
    {i, ["priv/mibs"]},
    {outdir, "priv/mibs"}
]}.
```

## shell

**Purpose**: Rebar3 shell configuration

**Type**: `[{Key, Value}]`

**Example**:

```erlang
{shell, [
    {config, "config/sys.config"},
    {apps, [my_app]},
    {script_file, "scripts/shell.escript"}
]}.
```
