# Configuration

## Dependency Specification Formats

### Package Dependency (Hex)

**Simple**:

```erlang
{deps, [
    jsx                          % Latest version
]}.
```

**With Version**:

```erlang
{deps, [
    {jsx, "3.1.0"},             % Exact version
    {jsx, "~> 3.1"},            % Semantic versioning
    {jsx, ">= 2.0.0"}           % Version constraint
]}.
```

### Source Dependency (Git)

```erlang
{deps, [
    {cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.9.0"}}},
    {ranch, {git, "git://github.com/ninenines/ranch.git", {branch, "master"}}},
    {gun, {git, "https://github.com/ninenines/gun.git", {ref, "abc123"}}}
]}.
```

### Source Dependency (Mercurial)

```erlang
{deps, [
    {my_dep, {hg, "https://bitbucket.org/user/my_dep", {tag, "1.0.0"}}}
]}.
```

### Checkout Dependency

**Not in `rebar.config`**. Created by placing dependency in `_checkouts/` directory.

**Behavior**:

- Always compiled
- Never locked
- Overrides any version specified in config

## Profile-Specific Dependencies

```erlang
{profiles, [
    {test, [
        {deps, [
            meck,
            proper
        ]}
    ]},
    {prod, [
        {deps, [
            recon
        ]}
    ]}
]}.
```

**Merging**:

- Profile deps added to default profile deps
- No removal; only addition
- Active profiles merge in order

## Lock File Configuration

**File**: `rebar.lock`

**Format Version**: `"1.2.0"` (current)

**Structure**:

```erlang
{"1.2.0",
[{<<"cowboy">>, {git, "https://github.com/ninenines/cowboy.git",
                      {ref, "abc123"}}, 0},
 {<<"jsx">>, {pkg, <<"jsx">>, <<"3.1.0">>}, 0},
 {<<"ranch">>, {git, "https://github.com/ninenines/ranch.git",
                     {ref, "def456"}}, 1}]}.
[
{pkg_hash, [
  {<<"jsx">>, <<"ABCD1234...">>},
  {<<"cowlib">>, <<"EFGH5678...">>}
]}
].
```

**Components**:

1. Version string
2. List of lock entries (name, source, level)
3. List of package hashes (for integrity verification)
