# Example Scenarios

## Scenario 1: Simple Project Initialization

**Setup**:

```
my_app/
├── rebar.config
└── src/
    └── my_app.erl
```

**`rebar.config`**:

```erlang
{erl_opts, [debug_info]}.
```

**Execution**: `rebar3 compile`

**Flow**:

1. Read `rebar.config` → `[{erl_opts, [debug_info]}]`
2. No `rebar.lock` → empty lock
3. Create state with opts
4. Apply default profile
5. No plugins to install
6. Register built-in providers
7. Dispatch to compile command

**Result**: State initialized with debug_info enabled

---

## Scenario 2: Project with Test Profile

**`rebar.config`**:

```erlang
{erl_opts, [debug_info]}.
{profiles, [
    {test, [
        {deps, [meck]},
        {erl_opts, [nowarn_export_all]}
    ]}
]}.
```

**Execution**: `rebar3 as test compile`

**Flow**:

1. Load base config
2. Apply `default` profile
3. Apply `test` profile:
   - Add `meck` to deps
   - Add `nowarn_export_all` to `erl_opts`
4. Result: `{erl_opts, [nowarn_export_all, debug_info]}`

---

## Scenario 3: Global Plugin Installation

**`~/.config/rebar3/rebar.config`**:

```erlang
{plugins, [rebar3_hex]}.
```

**Execution**: `rebar3 compile` (in any project)

**Flow**:

1. Load project config
2. Detect global config exists
3. Load global config
4. Install `rebar3_hex` plugin globally
5. Register hex providers (publish, etc.)
6. Proceed with project compilation

**Result**: Hex commands available in all projects

---

## Scenario 4: Environment Variable Override

**`rebar.config`**:

```erlang
{erl_opts, [debug_info]}.
{profiles, [
    {prod, [{erl_opts, [no_debug_info]}]}
]}.
```

**Execution**: `REBAR_PROFILE=prod rebar3 compile`

**Flow**:

1. Load base config
2. Detect `REBAR_PROFILE=prod`
3. Apply `prod` profile
4. Result: `{erl_opts, [no_debug_info]}`

---

## Scenario 5: Offline Mode

**Execution**: `rebar3 compile --offline`

**Flow**:

1. Parse `--offline` flag
2. Set `REBAR_OFFLINE=1` environment variable
3. Skip starting `ssl` and `inets`
4. Set `offline` flag in state
5. Later stages skip network operations

**Impact**: Dependencies must already be cached
