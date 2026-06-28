# lfe_types.erl - Type System Support

**Purpose**: Parse and convert type specifications for use with Dialyzer.

**Location**: `src/lfe_types.erl`
**Size**: 444 LOC, 16KB

**Module Classification**: Library support, type system

#### Public API

```erlang
to_type_def(TypeDef, Line) -> ErlangTypeAST
to_func_spec(Spec, Line) -> ErlangSpecAST
```

Convert LFE type definitions to Erlang type AST. Located at `lfe_types.erl:52-87`.

#### Type Syntax

**LFE Type Definitions**:

```lisp
(deftype int () integer)

(deftype list (a) (list a))

(deftype pair (a b) (tuple a b))

(deftype tree (a)
  (union (tuple 'leaf a)
         (tuple 'node (tree a) (tree a))))
```

**Function Specs**:

```lisp
(defun add
  "Add two numbers"
  {(spec [[integer integer] integer])}
  ([x y] (+ x y)))

; Multiple arities
(defun process
  {(spec [[atom] ok]
         [[atom list] {ok value}])}
  ([cmd] ...)
  ([cmd args] ...))
```

#### Type Constructors

**Built-in Types**:

```
any, none, atom, integer, float, number, boolean,
binary, bitstring, list, tuple, map, pid, port, reference,
function, ...
```

**Constructors**:

```
(list TYPE)                  ; List of TYPE
(tuple TYPE ...)             ; Tuple with types
(map KEY-TYPE VAL-TYPE)      ; Map
(union TYPE ...)             ; Sum type
(fun ARGS RESULT)            ; Function type
(record NAME FIELDS)         ; Record type
```

#### Translation to Erlang

**Type Definition Translation** (`to_type_def/2` at lines 134-256):

```lisp
; LFE
(deftype pair (a b) (tuple a b))

; Erlang AST
-type pair(A, B) :: {A, B}.
```

**Spec Translation** (`to_func_spec/2` at lines 298-387):

```lisp
; LFE
{(spec [[integer integer] integer])}

; Erlang AST
-spec func(integer(), integer()) -> integer().
```

#### Dialyzer Integration

Type specs are:

1. Converted to Erlang AST
2. Stored in module attributes
3. Included in debug_info
4. Read by Dialyzer for type checking

**Zero Runtime Cost**: Types are compile-time only.

#### Dependencies

**LFE modules**:

- `lfe_internal` - Type validation

**Erlang compiler**:

- `erl_parse` - Erlang AST

#### Used By

- `lfe_lint` - Type checking during linting
- `lfe_codegen` - Generating type attributes
- Dialyzer - Type analysis

#### Special Considerations

**Optional**: Types are optional in LFE (like Erlang).

**Gradual Typing**: Can add types incrementally to codebase.

**Dialyzer Only**: Types are for static analysis, not runtime checking.
