# Stage 3: Macro Expansion (lfe_macro.erl)

**Purpose**: Recursively expand all macros, transforming user-level syntax into core forms.

**Module**: `lfe_macro.erl` (1,432 LOC) - Location: `src/lfe_macro.erl`

This is the **heart of LFE's power**. The macro system enables metaprogramming, DSL creation, and syntactic abstraction.

**Macro state**:

```erlang
-record(mac, {
    deep = true,                % Deep expand everything
    keep = true,                % Keep all forms
    module = '-no-module-',     % Current module
    line = 1,                   % Line no of current form
    vc = 0,                     % Variable counter
    fc = 0,                     % Function counter
    file = [],                  % File name
    opts = [],                  % Compiler options
    ipath = [],                 % Include path
    errors = [],                % Errors
    warnings = [],              % Warnings
    unloadable = []             % Macro modules we can't load
}).
```

**Expansion strategy** (from `pass_form/3` at src/lfe_macro.erl:180-213):

```erlang
1. Handle progn → recursively expand all forms
2. Handle eval-when-compile → evaluate at compile time
3. Handle include-file/include-lib → file inclusion
4. Handle define-macro → collect macro definitions
5. Expand other forms recursively
```

**Macro types**:

**1. User-defined macros** (from `define-macro`):

```lisp
(define-macro my-when (test . body)
  `(if ,test (progn ,@body) 'false))
```

Stored in environment as `{macro, Definition}` where Definition is a lambda or match-lambda that receives `[Args, $ENV]`.

**2. Built-in macros** (50+ total, from `exp_predef/3` at src/lfe_macro.erl:290-800):

```lisp
;; Common Lisp style
defmodule, defun, defmacro

;; Convenience
cond, let*, flet*, list*

;; Data structures
defrecord, defstruct

;; Syntactic sugar
:module:function → (call 'module 'function ...)

;; Pattern matching
match-spec (ETS/trace DSL)

;; Query comprehensions
qlc

;; Special
MODULE, LINE (compile-time constants)
```

**Backquote (quasiquotation) expansion**:

From `exp_backquote/2` (src/lfe_macro.erl:1343-1408):

Implements R6RS-compliant quasiquotation:

```lisp
`(a ,b ,@c)  →  [list, [quote, a], b | c]
```

**Algorithm**:

```text
1. Scan expression for comma and comma-at
2. Build list construction for comma (unquote)
3. Build list splicing for comma-at (unquote-splicing)
4. Handle nested backquotes recursively
5. Special handling for tuples and maps
```

**Hygiene mechanism**:

LFE macros are **unhygienic** (like Common Lisp, unlike Scheme). **LFE does not provide `gensym`** - there is no built-in facility for generating unique symbols.

**Manual hygiene techniques**:

1. Use sufficiently unique variable names in macro-generated code
2. Rely on lexical scoping to avoid most capture issues
3. Use the `$ENV` parameter to check for name conflicts at expansion time
4. For Scheme-style hygiene, use the optional `scm` module (see below)

**Macro expansion order**:

```text
1. Top-level forms expanded first
2. Nested forms expanded recursively (inside-out)
3. User macros shadow built-in macros
4. Most recently defined macro wins (LIFO)
```
