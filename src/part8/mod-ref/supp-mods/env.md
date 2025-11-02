# lfe_env.erl - Environment Management

**Purpose**: Manage lexical environments for variable, function, macro, and record bindings.

**Location**: `src/lfe_env.erl`
**Size**: 252 LOC, 8.4KB

**Module Classification**: Infrastructure, state management

This module is **heavily used** throughout the codebase (imported by `lfe_macro`, used by `lfe_eval`, `lfe_lint`, `lfe_comp`).

#### Public API

**Environment Creation**:

```erlang
new() -> Env
add_env(Env1, Env2) -> Env
```

Create and merge environments. Lines 111-124.

**Variables**:

```erlang
add_vbinding(Name, Value, Env) -> Env
add_vbindings([{Name, Value}], Env) -> Env
is_vbound(Name, Env) -> boolean()
get_vbinding(Name, Env) -> {yes, Value} | no
fetch_vbinding(Name, Env) -> Value
del_vbinding(Name, Env) -> Env
```

Variable bindings. Lines 169-189.

**Functions**:

```erlang
add_fbinding(Name, Arity, Value, Env) -> Env
add_fbindings([{Name, Arity, Value}], Env) -> Env
is_fbound(Name, Arity, Env) -> boolean()
get_fbinding(Name, Arity, Env) -> {yes, Value} | {yes, Mod, Name} | no
add_ibinding(Mod, RemoteName, Arity, LocalName, Env) -> Env
```

Function bindings (including imports). Lines 193-233.

**Macros**:

```erlang
add_mbinding(Name, Macro, Env) -> Env
add_mbindings([{Name, Macro}], Env) -> Env
is_mbound(Name, Env) -> boolean()
get_mbinding(Name, Env) -> {yes, Macro} | no
```

Macro bindings. Lines 237-255.

**Records**:

```erlang
add_record(Name, Fields, Env) -> Env
get_record(Name, Env) -> {yes, Fields} | no
```

Record definitions. Lines 259-266.

#### Environment Structure

**Record** (lines 62-63):

```erlang
-record(env, {
    vars=null,  % Variable bindings (map/orddict)
    funs=null,  % Function/macro bindings (map/orddict)
    recs=null   % Record definitions (map/orddict)
}).
```

**Conditional Implementation** (lines 37-60):

Uses **maps** if available, otherwise **orddict**:

```erlang
-ifdef(HAS_MAPS).
-define(NEW(), #{}).
-define(GET(K, D), maps:get(K, D)).
-define(PUT(K, V, D), maps:put(K, V, D)).
...
-else.
-define(NEW(), orddict:new()).
-define(GET(K, D), orddict:fetch(K, D)).
-define(PUT(K, V, D), orddict:store(K, V, D)).
...
-endif.
```

**Why?** Backward compatibility with older Erlang versions.

#### Function/Macro Shadowing

**Key Design** (lines 103-110):

Functions and macros share the same namespace, with shadowing rules:

- Defining a macro shadows all functions with that name
- Defining a function shadows a macro with that name
- Functions with different arities coexist

**Storage**:

```erlang
Funs = #{
    foo => {macro, MacroDef},
    bar => {function, [{1, Fun1}, {2, Fun2}]}
}
```

#### Dependencies

- `lists` (for fold operations)
- Conditional: `maps` or `orddict`

#### Used By

- **lfe_macro** - **IMPORTS** this module (only module to do so!)
- `lfe_eval` - Heavy use (39 calls)
- `lfe_lint` - Heavy use
- `lfe_comp` - Moderate use
- All modules doing evaluation or compilation

#### Special Considerations

**Performance Critical**: Environment operations are hot paths.

**Thread Safety**: Environments are immutable (functional updates).

**Scoping**: Supports lexical scoping via environment chaining.
