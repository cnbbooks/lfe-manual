# lfe_ms.erl - Match Specification DSL

**Purpose**: Provide a DSL for creating Erlang match specifications used in ETS, Mnesia, and tracing.

**Location**: `src/lfe_ms.erl`
**Size**: 473 LOC, 16KB

**Module Classification**: Library, DSL

#### Public API

```erlang
ets_transform(Spec) -> MatchSpec
trace_transform(Spec) -> MatchSpec
```

Transform LFE match spec DSL to Erlang match spec. Located at `lfe_ms.erl:48-65`.

#### Match Specification DSL

**Erlang Match Specs** (complex):

```erlang
% Match ETS tuples where first element > 10, return second element
[{{'$1', '$2', '_'}, [{'>', '$1', 10}], ['$2']}]
```

**LFE DSL** (readable):

```lisp
(ets-ms
  ([(tuple a b _)]
   (when (> a 10))
   b))
```

**Components**:

1. **Head Pattern**: Match against input
2. **Guards**: Filter matches
3. **Body**: Transform result

#### ETS Match Specs

**Examples**:

```lisp
; Select all
(ets-ms ([x] x))

; Select with guard
(ets-ms
  ([x] (when (> x 10)) x))

; Transform result
(ets-ms
  ([(tuple k v)]
   (tuple v k)))   ; Swap key/value

; Multiple clauses
(ets-ms
  ([(tuple 'foo v)] v)
  ([(tuple 'bar v)] (* v 2))
  ([_] 'undefined))
```

#### Trace Match Specs

**Tracing Function Calls**:

```lisp
(trace-ms
  ([[arg1 arg2]]            ; Match arguments
   (when (> arg1 100))      ; Guard
   (return_trace)))         ; Action
```

**Actions**:

- `(return_trace)` - Trace return value
- `(message MSG)` - Send message
- `(exception_trace)` - Trace exceptions

#### Implementation

**Transformation** (`transform/2` at lines 123-278):

Converts DSL to Erlang match spec format:

1. Parse head pattern → `MatchHead`
2. Parse guards → `[ConditionList]`
3. Parse body → `[Body]`
4. Return: `[{MatchHead, [ConditionList], [Body]}]`

**Variable Mapping** (`assign_vars/1` at lines 302-345):

Maps LFE variables to match spec variables (`$1`, `$2`, ...).

**Guard Conversion** (`convert_guard/1` at lines 378-421):

Converts LFE guards to match spec guard expressions.

#### Dependencies

**LFE modules**:

- `lfe_macro` - Used by callers for macro expansion

**Erlang stdlib**:

- `lists`, `orddict`

#### Used By

- User code via `ets-ms` and `trace-ms` macros
- Advanced ETS/Mnesia queries
- Tracing and debugging

#### Special Considerations

**Compile-Time**: Match specs generated at compile time (zero runtime overhead).

**Type Safety**: No runtime type checking; invalid specs cause ETS/trace errors.

**Power**: Match specs are very powerful but complex; DSL makes them accessible.
