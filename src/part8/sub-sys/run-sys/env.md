# Environment Management (lfe_env.erl)

**Purpose**: Manage variable, function, macro, and record bindings.

**Module**: `lfe_env.erl` (252 LOC) - Location: `src/lfe_env.erl`

**Environment structure**:

```erlang
-record(env, {
    vars = null,    % Variable bindings (map or orddict)
    funs = null,    % Function/macro bindings (map or orddict)
    recs = null     % Record definitions (map or orddict)
}).
```

**Implementation**:

Conditional compilation based on map availability:

```erlang
-ifdef(HAS_MAPS).
    % Use maps module
-else.
    % Use orddict
-endif.
```

**Variable operations**:

```erlang
add_vbinding(Name, Value, Env) -> Env
is_vbound(Name, Env) -> boolean()
get_vbinding(Name, Env) -> {yes, Value} | no
fetch_vbinding(Name, Env) -> Value  % throws if not found
del_vbinding(Name, Env) -> Env
add_vbindings([{Name, Value}], Env) -> Env  % Bulk add
```

**Function operations**:

Functions stored as `{function, [{Arity, Definition}]}`:

```erlang
add_fbinding(Name, Arity, Value, Env) -> Env
is_fbound(Name, Arity, Env) -> boolean()
get_fbinding(Name, Arity, Env) ->
    {yes, Value} |              % Local function
    {yes, Mod, Func} |         % Imported function
    no
```

Imported functions stored as `{Arity, Module, RemoteName}`.

**Macro operations**:

Macros stored as `{macro, Definition}`:

```erlang
add_mbinding(Name, Macro, Env) -> Env
is_mbound(Name, Env) -> boolean()
get_mbinding(Name, Env) -> {yes, Macro} | no
```

**Important**: Adding a macro **shadows ALL function definitions** with that name (because macros are expanded at compile-time).

**Record operations**:

```erlang
add_record(Name, Fields, Env) -> Env
get_record(Name, Env) -> {yes, Fields} | no
```

**Environment merging**:

```erlang
add_env(Env1, Env2) -> Env
```

Merges two environments, preferring `Env1` bindings in case of conflicts.

**Environment threading**:

Environments are **immutable**:

```erlang
Env0 = lfe_env:new(),
Env1 = lfe_env:add_vbinding(x, 42, Env0),
Env2 = lfe_env:add_vbinding(y, 100, Env1),
% Env0, Env1 unchanged
```

This enables:

- Safe concurrency
- Easy backtracking
- Clear scoping rules
