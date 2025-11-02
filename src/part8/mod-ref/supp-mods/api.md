# lfe.erl - Public API Facade

**Purpose**: Provide a simple, stable public API for common LFE operations.

**Location**: `src/lfe.erl`
**Size**: 143 LOC, 4.9KB

**Module Classification**: API facade, entry point

#### Public API

**Compilation**:

```erlang
compile(File) -> Result
compile(File, Options) -> Result
```

Compile LFE file. Delegates to `lfe_comp:file/2`.

**Evaluation**:

```erlang
eval(Sexpr) -> Value
eval(Sexpr, Env) -> Value
```

Evaluate expression. Delegates to `lfe_eval:expr/1,2`.

**Version**:

```erlang
version() -> string()
```

Return LFE version.

#### Facade Pattern

**Purpose**: Stable interface insulating users from internal module changes.

**Example**:

```erlang
% User code (stable)
lfe:eval('(+ 1 2)').

% Instead of (unstable)
lfe_eval:expr('(+ 1 2)', lfe_env:new()).
```

#### Dependencies

**LFE modules**:

- `lfe_comp` - Compilation
- `lfe_eval` - Evaluation

#### Used By

- User code
- External tools
- Documentation examples

#### Special Considerations

**Stability**: This API rarely changes (backward compatibility).

**Convenience**: Provides sensible defaults for common operations.

**Minimal**: Only most common operations exposed.
