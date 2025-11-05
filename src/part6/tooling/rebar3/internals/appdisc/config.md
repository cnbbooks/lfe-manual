# Configuration

## Library Directories

**Configuration Key**: `lib_dirs`

**Default**: `[]`

**Purpose**: Additional directories to search for applications

**Example**:

```erlang
{lib_dirs, ["apps", "my_libs"]}.
```

**Result**: Searches `apps/*/` and `my_libs/*/` for applications

---

## Source Directories

**Configuration Key**: `src_dirs`

**Default**: `["src"]`

**Purpose**: Directories within each app containing source files

**Example**:

```erlang
{src_dirs, ["src", "lib"]}.
```

**Result**: Searches `APP/src/` and `APP/lib/` for `.app.src` files

---

## Application Resource Extensions

**Configuration Key**: `application_resource_extensions`

**Default**: `[".app", ".app.src", ".app.src.script"]`

**Purpose**: File extensions to recognize as application resources

**Rarely Changed**: Standard OTP conventions

---

## Application Structure

### Single-App Project

```
my_app/
├── rebar.config
├── src/
│   ├── my_app.app.src
│   └── *.erl
└── include/
    └── *.hrl
```

**Discovery**: One app found at root → single-app project

---

### Umbrella Project

```
my_project/
├── rebar.config
└── apps/
    ├── app1/
    │   ├── src/
    │   │   ├── app1.app.src
    │   │   └── *.erl
    ├── app2/
    │   └── src/
    │       ├── app2.app.src
    │       └── *.erl
```

**Discovery**: Multiple apps in `apps/` → umbrella project

---

## Application Resource File Format

### `.app.src`

**Location**: `src/APP.app.src`

**Format**: Erlang term

**Example**:

```erlang
{application, my_app, [
    {description, "My Application"},
    {vsn, "1.0.0"},
    {registered, []},
    {applications, [kernel, stdlib]},
    {mod, {my_app_app, []}},
    {env, []}
]}.
```

---

### `.app`

**Location**: `ebin/APP.app`

**Format**: Same as `.app.src` but with modules list

**Generated**: Usually created from `.app.src` during compilation

---

### `.app.src.script`

**Location**: `src/APP.app.src.script`

**Format**: Erlang code that returns application term

**Example**:

```erlang
case os:getenv("PROD") of
    "true" ->
        {application, my_app, [{vsn, "1.0.0"}, ...]};
    _ ->
        {application, my_app, [{vsn, "dev"}, ...]}
end.
```
