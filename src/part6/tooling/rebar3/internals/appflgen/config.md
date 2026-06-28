# Configuration

## Application Source File Format

**File**: `src/APP.app.src`

**Structure**:

```erlang
{application, my_app, [
    {description, "My Application"},
    {vsn, "1.0.0"},
    {registered, [my_app_sup]},
    {applications, [kernel, stdlib, sasl]},
    {included_applications, []},
    {mod, {my_app_app, []}},
    {env, [
        {config_key, default_value}
    ]},
    {modules, []},  % Filled automatically
    {licenses, ["Apache 2.0"]},
    {links, [{"GitHub", "https://github.com/user/my_app"}]}
]}.
```

## Required Fields

| Field | Required | Default if Missing |
|-------|----------|-------------------|
| `description` | Yes | `""` (added automatically) |
| `vsn` | Yes | Error if missing |
| `registered` | Yes | `[]` (added automatically) |
| `applications` | Yes | Error if missing |
| `modules` | No | Generated automatically |
| `mod` | No | None (library app) |

## Variable Substitution

**Syntax**: Use atoms as placeholders

**Example**:

```erlang
% .app.src:
{application, my_app, [
    {vsn, "1.0.0"},
    {modules, modules},  % Placeholder
    {custom_field, custom_value}  % Placeholder
]}.

% Substitution:
AppVars = [
    {modules, [mod1, mod2, mod3]},
    {custom_value, "Actual Value"}
]

% Result in .app:
{application, my_app, [
    {vsn, "1.0.0"},
    {modules, [mod1, mod2, mod3]},
    {custom_field, "Actual Value"}
]}.
```

## App Vars File

**Configuration**:

```erlang
{app_vars_file, "config/app.vars"}.
```

**File Format** (Erlang terms):

```erlang
{copyright, "Copyright (c) 2024"}.
{build_date, "2024-01-15"}.
{custom_value, some_atom}.
```

## Validate App Modules

**Configuration**:

```erlang
{validate_app_modules, true}.  % Default
{validate_app_modules, false}. % Skip validation
```

**When to Disable**:

- Dynamic module loading
- Modules generated at runtime
- NIF-based applications with special requirements
