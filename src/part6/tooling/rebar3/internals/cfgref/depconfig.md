# Dependency Configuration

## deps

**Purpose**: Project dependencies

**Type**: `[Dep]` where `Dep` is:

- `atom()`: Package from Hex (latest version)
- `{atom(), binary()}`: Package with version
- `{atom(), VCS}`: From version control

**Examples**:

```erlang
{deps, [
    % Hex packages
    jsx,                                    % Latest
    {cowboy, "2.9.0"},                      % Specific version
    {cowlib, "~> 2.11"},                    % Semver constraint

    % Git
    {my_dep, {git, "https://github.com/user/my_dep.git", {tag, "1.0.0"}}},
    {my_dep, {git, "...", {branch, "main"}}},
    {my_dep, {git, "...", {ref, "abc123"}}},

    % Mercurial
    {my_dep, {hg, "https://bitbucket.org/user/my_dep", {tag, "1.0.0"}}}
]}.
```

## overrides

**Purpose**: Override dependency configuration

**Types**:

- `{override, Dep, Config}`: Override specific dep
- `{add, Dep, Config}`: Add to dep's config
- `{del, Dep, Config}`: Delete from dep's config

**Examples**:

```erlang
{overrides, [
    % Override specific dependency
    {override, cowboy, [
        {erl_opts, [nowarn_export_all]}
    ]},

    % Add to dependency config
    {add, ranch, [
        {erl_opts, [debug_info]}
    ]},

    % Delete from dependency config
    {del, jsx, [
        {erl_opts, [warnings_as_errors]}
    ]}
]}.
```
