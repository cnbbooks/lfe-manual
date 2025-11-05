# Example Scenarios

## Scenario 1: Single-App Project

**Structure**:

```
my_app/
├── rebar.config
└── src/
    ├── my_app.app.src
    └── my_app.erl
```

**Discovery**:

1. Scan root directory
2. Find `src/my_app.app.src`
3. Parse app resource
4. App dir = root directory
5. `define_root_app` returns `<<"my_app">>`
6. Single-app project detected

**Result**: One application in `project_apps`

---

## Scenario 2: Umbrella Project

**Structure**:

```
my_project/
├── rebar.config
└── apps/
    ├── web/
    │   └── src/web.app.src
    └── db/
        └── src/db.app.src
```

**Discovery**:

1. Scan `apps/` directory (from `lib_dirs`)
2. Find `apps/web/src/web.app.src`
3. Find `apps/db/src/db.app.src`
4. Parse both app resources
5. `define_root_app` returns `root` (no app at root)
6. Umbrella project detected

**Result**: Two applications in `project_apps`

---

## Scenario 3: Custom Source Directories

**`rebar.config`**:

```erlang
{src_dirs, ["src", "lib", "core"]}.
```

**Structure**:

```
my_app/
├── src/my_app.app.src
├── lib/helper.erl
└── core/engine.erl
```

**Discovery**:

1. Search `src/`, `lib/`, `core/` for app resources
2. Find `src/my_app.app.src`
3. Associate all three directories with this app

**Result**: App discovered with multiple source directories

---

## Scenario 4: Mix.exs Compatibility

**Structure**:

```
my_elixir_app/
├── mix.exs
└── lib/
    └── my_elixir_app.ex
```

**Discovery**:

1. Find `mix.exs`
2. Parse Elixir project configuration
3. Extract application metadata
4. Create app info compatible with rebar3

**Result**: Elixir app discoverable by rebar3

---

## Scenario 5: Application-Specific Dependencies

**`apps/web/rebar.config`**:

```erlang
{deps, [cowboy]}.
```

**`apps/db/rebar.config`**:

```erlang
{deps, [epgsql]}.
```

**Discovery**:

1. Discover both apps
2. Read each app's `rebar.config`
3. Merge deps: `web` gets `cowboy`, `db` gets `epgsql`
4. Top-level deps also added to both

**Result**: Each app has appropriate dependencies
