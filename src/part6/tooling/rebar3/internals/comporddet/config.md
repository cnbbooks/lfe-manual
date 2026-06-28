# Configuration

## No Direct Configuration

This stage doesn't have configuration options. However, the dependency information it uses comes from:

**From `.app` or `.app.src`**:

```erlang
{applications, [kernel, stdlib, dependency1, dependency2]}
```

**From `rebar.config`**:

```erlang
{deps, [
    dependency3,
    {dependency4, "1.0.0"}
]}.
```

## Dependency Types That Affect Ordering

**Runtime Dependencies** (`applications` in `.app`):

- Required for application to run
- Must be loaded before application starts
- Affect compilation order (may need headers)

**Build Dependencies** (`deps` in `rebar.config`):

- Required for compilation
- Parse transforms, behaviors, include files
- Always affect compilation order

**Example**:

```erlang
% In my_app.app.src:
{applications, [kernel, stdlib, cowboy]}.

% In rebar.config:
{deps, [{cowboy, "2.9.0"}]}.
```

Both sources contribute to dependency graph for compilation ordering.
