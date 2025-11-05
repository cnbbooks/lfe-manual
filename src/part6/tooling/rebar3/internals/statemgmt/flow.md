# State Flow

## Through Compilation Chain

```
[Initialization]
   ↓ rebar_state:new(Config)
   State: {dir, opts, ...}

[Dependency Resolution]
   ↓ Update with resolved deps
   State: {..., all_deps, deps_to_build}

[Application Discovery]
   ↓ Add discovered apps
   State: {..., project_apps}

[Compilation]
   ↓ For each app
   State: {..., current_app}
   ↓ Compile sources
   ↓ Generate .app
   State: {..., updated project_apps}

[Final State]
   State with all apps compiled
```

## State Immutability

**Pattern**: Always return new state

```erlang
% GOOD:
{ok, NewState} = provider:do(State)

% BAD (not possible):
provider:do_mutate(State)  % No in-place mutation
```

## State Threading

**Example through providers**:

```erlang
State0 = rebar3:init_config(),
{ok, State1} = rebar_prv_app_discovery:do(State0),
{ok, State2} = rebar_prv_install_deps:do(State1),
{ok, State3} = rebar_prv_compile:do(State2)
```
