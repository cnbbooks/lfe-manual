# Configuration

## Configuration Files

### `rebar.config`

**Location**: Project root directory

**Format**: Erlang terms

**Common Options**:

```erlang
{erl_opts, [debug_info, warnings_as_errors]}.
{deps, [
    {jsx, "3.1.0"},
    {cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.9.0"}}}
]}.
{plugins, [rebar3_hex]}.
{profiles, [
    {test, [
        {deps, [meck, proper]},
        {erl_opts, [{d, 'TEST'}]}
    ]}
]}.
```

### `rebar.lock`

**Location**: Project root directory

**Format**: Versioned Erlang terms

**Purpose**: Lock dependency versions for reproducible builds

**Example**:

```erlang
{"1.2.0",
[{<<"jsx">>, {pkg,<<"jsx">>,<<"3.1.0">>}, 0}]}.
[
{pkg_hash,[
 {<<"jsx">>, <<"...">>}]}
].
```

### `~/.config/rebar3/rebar.config`

**Location**: User's global configuration directory

**Purpose**: Global settings and plugins

**Common Uses**:

- Global plugins (hex, dialyzer, etc.)
- Hex repository credentials
- Global compiler options

## Environment Variables

| Variable | Effect | Default |
|----------|--------|---------|
| `REBAR_PROFILE` | Override active profile | none |
| `REBAR_BASE_DIR` | Build output directory | `_build` |
| `REBAR_CACHE_DIR` | Global cache location | `~/.cache/rebar3` |
| `REBAR_OFFLINE` | Disable network access | not set |
| `REBAR_SKIP_PROJECT_PLUGINS` | Skip project plugin installation | not set |
| `HEX_CDN` | Hex package CDN URL | default Hex CDN |
| `QUIET` | Minimal output | not set |
| `DEBUG` | Verbose debugging output | not set |
| `DIAGNOSTIC` | Very verbose output | not set |

## Command-Line Options

Global options (available for all commands):

- `-h`, `--help`: Show help
- `-v`, `--version`: Show version information
- `--offline`: Run in offline mode (no network access)

## Configuration Precedence

Configuration is merged in the following order (later overrides earlier):

1. Built-in defaults
2. `~/.config/rebar3/rebar.config` (global config)
3. Project `rebar.config`
4. Profile-specific configuration
5. Environment variables
6. Command-line options
