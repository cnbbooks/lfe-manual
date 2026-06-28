# Functions & API Calls

## `rebar3:main/1`

**Purpose**: Entry point from escript

**Signature**:

```erlang
-spec main([string()]) -> no_return().
```

**Arguments**:

- `Args` (`[string()]`): Command-line arguments

**Returns**: Does not return; calls `erlang:halt/1`

**Flow**:

- Calls `rebar3:run/1`
- Handles success/error
- Exits with appropriate code

---

## `rebar3:run/1`

**Purpose**: Main execution entry point from command line

**Signature**:

```erlang
-spec run([string()]) -> {ok, rebar_state:t()} | {error, term()}.
```

**Arguments**:

- `RawArgs` (`[string()]`): Command-line arguments

**Returns**:

- `{ok, State}`: Successful execution with final state
- `{error, Reason}`: Error occurred

**Flow**:

1. Start and load applications
2. Call `init_config/0` to create base state
3. Set `caller` to `command_line`
4. Call `set_options/2` to process global flags
5. Call `run_aux/2` for actual execution

---

## `rebar3:init_config/0`

**Purpose**: Set up base configuration and initial state

**Signature**:

```erlang
-spec init_config() -> rebar_state:t().
```

**Returns**: Initialized state record

**Flow**:

1. Set HTTPC options via `rebar_utils:set_httpc_options/0`
2. Initialize logging via `rebar_log:init/2`
3. Read `rebar.config` via `rebar_config:consult_root/0`
4. Read `rebar.lock` via `rebar_config:consult_lock_file/1`
5. Merge locks into config via `rebar_config:merge_locks/2`
6. Create state via `rebar_state:new/1`
7. Load global config if exists
8. Set escript path
9. Initialize vsn cache

**Example Usage**:

```erlang
BaseState = rebar3:init_config()
```

---

## `rebar_config:consult_root/0`

**Purpose**: Read the main `rebar.config` file

**Signature**:

```erlang
-spec consult_root() -> [term()].
```

**Returns**: List of configuration terms

**Flow**:

- Looks for `rebar.config` in current directory
- Parses as Erlang terms
- Returns empty list if file doesn't exist

**Called From**: [Initialization & Configuration](/part6/tooling/rebar3/internals/init-cfg/)

---

## `rebar_config:consult_lock_file/1`

**Purpose**: Read and parse the lock file

**Signature**:

```erlang
-spec consult_lock_file(file:filename()) -> [term()].
```

**Arguments**:

- `File` (`file:filename()`): Path to `rebar.lock`

**Returns**: List of lock entries in internal format

**Flow**:

1. Read lock file
2. Detect version (beta, "1.2.0", etc.)
3. Parse locks based on version
4. Extract package hashes
5. Expand locks with hash information
6. Return internal lock format

**Lock File Versions**:

- Beta format: `[Locks].`
- Versioned format: `{"1.2.0", Locks}. [Attrs].`

---

## `rebar_state:new/0,1,2,3`

**Purpose**: Create a new rebar state record

**Signature**:

```erlang
-spec new() -> t().
-spec new(list()) -> t().
-spec new(t() | atom(), list()) -> t().
-spec new(t(), list(), file:filename_all()) -> t().
```

**Arguments** (for `new/1`):

- `Config` (`list()`): Configuration terms from `rebar.config`

**Returns**: New state record

**Flow** (for `new/1`):

1. Convert config list to dict via `base_opts/1`
2. Create base state with opts
3. Set current working directory
4. Set default opts

**State Record**:

```erlang
-record(state_t, {
    dir                               :: file:name(),
    opts                = dict:new()  :: rebar_dict(),
    code_paths          = dict:new()  :: rebar_dict(),
    default             = dict:new()  :: rebar_dict(),
    escript_path                      :: undefined | file:filename_all(),
    lock                = [],
    current_profiles    = [default]   :: [atom()],
    namespace           = default     :: atom(),
    command_args        = [],
    command_parsed_args = {[], []},
    current_app                       :: undefined | rebar_app_info:t(),
    project_apps        = []          :: [rebar_app_info:t()],
    deps_to_build       = []          :: [rebar_app_info:t()],
    all_plugin_deps     = []          :: [rebar_app_info:t()],
    all_deps            = []          :: [rebar_app_info:t()],
    compilers           = []          :: [module()],
    project_builders    = []          :: [{project_type(), module()}],
    resources           = [],
    providers           = [],
    allow_provider_overrides = false  :: boolean()
}).
```

---

## `rebar_state:apply_profiles/2`

**Purpose**: Apply configuration profiles to state

**Signature**:

```erlang
-spec apply_profiles(t(), [atom()]) -> t().
```

**Arguments**:

- `State` (`t()`): Current state
- `Profiles` (`[atom()]`): List of profiles to apply (e.g., `[test]`, `[prod]`)

**Returns**: State with merged profile configuration

**Flow**:

1. Get profiles config from state
2. For each profile (except `default`):
   - Get profile-specific options
   - Merge with current opts using `rebar_opts:merge_opts/3`
3. Update `current_profiles` field
4. Return modified state

**Profile Merging**:

- Base configuration is from `default` profile
- Profile-specific configs override base
- Multiple profiles stack in order given

---

## `rebar_plugins:project_plugins_install/1`

**Purpose**: Install plugins specified in `rebar.config`

**Signature**:

```erlang
-spec project_plugins_install(rebar_state:t()) -> rebar_state:t().
```

**Arguments**:

- `State` (`rebar_state:t()`): Current state

**Returns**: State with plugins installed and providers registered

**Flow**:

1. Get `project_plugins` from configuration
2. For each plugin:
   - Fetch plugin (from Hex or Git)
   - Compile plugin
   - Load plugin application
   - Discover and register providers from plugin
3. Update state with new providers

---

## `rebar_state:create_logic_providers/2`

**Purpose**: Register built-in providers with the state

**Signature**:

```erlang
-spec create_logic_providers([module()], t()) -> t().
```

**Arguments**:

- `Providers` (`[module()]`): List of provider modules
- `State` (`t()`): Current state

**Returns**: State with providers registered

**Flow**:

1. For each provider module:
   - Call `Module:init(State)` to get provider record
   - Add to providers list
2. Return updated state

**Built-in Providers** (from application env):

- `rebar_prv_app_discovery`
- `rebar_prv_compile`
- `rebar_prv_clean`
- `rebar_prv_install_deps`
- `rebar_prv_lock`
- And many more...

---

## `rebar_core:init_command/2`

**Purpose**: Initialize and dispatch to the requested command

**Signature**:

```erlang
-spec init_command(rebar_state:t(), atom()) -> {ok, rebar_state:t()} | {error, term()}.
```

**Arguments**:

- `State` (`rebar_state:t()`): Current state
- `Command` (`atom()`): Command to execute (e.g., `compile`, `test`)

**Returns**:

- `{ok, NewState}`: Command executed successfully
- `{error, Reason}`: Command failed

**Flow**:

1. Handle special commands (`do`, `as`)
2. Call `process_namespace/2` to resolve namespace
3. Call `process_command/2` to execute provider chain
