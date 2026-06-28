# scm.erl - Scheme Compatibility

**Purpose**: Provide Scheme-style `syntax-rules` macro system for pattern-based macros.

**Location**: `src/scm.erl`
**Size**: 276 LOC, 9.3KB

**Module Classification**: Compatibility layer, Scheme syntax

## Public API

```erlang
define_syntax_rules(Name, Rules, Env) -> {[{Name, MacroFun}], Env}
```

Define a pattern-based macro. Located at `scm.erl:45-49`.

## Syntax Rules

**Scheme's `syntax-rules`**:

```scheme
; Scheme example
(define-syntax when
  (syntax-rules ()
    ((_ test body ...)
     (if test (begin body ...) #f))))
```

**LFE Equivalent**:

```lisp
(define-syntax when
  (syntax-rules ()
    ((_ test body ...)
     (if test (progn body ...) 'false))))
```

**Pattern Matching**: Matches macro call against patterns, expands according to template.

**Ellipsis (`...`)**: Represents zero or more repetitions.

## Implementation

**Pattern Compiler** (`compile_rule/2` at lines 98-156):

Converts syntax rules to LFE pattern-matching macro:

1. Parse pattern
2. Extract pattern variables
3. Generate match code
4. Generate template expansion code
5. Return lambda form

**Ellipsis Handling** (`expand_ellipsis/2` at lines 178-234):

Complex pattern matching for `...`:

```lisp
; Pattern: (foo a ...)
; Matches: (foo 1), (foo 1 2), (foo 1 2 3), ...
; Binds: a = [1], a = [1, 2], a = [1, 2, 3], ...
```

**Template Expansion** (`expand_template/2` at lines 256-298):

Substitutes pattern variables in template:

```lisp
; Template: (list a ...)
; With a = [1, 2, 3]
; Expands to: (list 1 2 3)
```

## Dependencies

**LFE modules**:

- `lfe_macro` - Macro expansion framework
- `lfe_env` - Environment management

**Erlang stdlib**:

- `lists`

## Used By

- User code wanting Scheme-style macros
- Porting Scheme code to LFE

## Special Considerations

**Limited Adoption**: Less commonly used than Clojure or CL styles in LFE.

**Power**: `syntax-rules` is very powerful for pattern-based macros.

**Hygiene**: LFE macros are unhygienic; `syntax-rules` doesn't change this.
