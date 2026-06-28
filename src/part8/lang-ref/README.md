# Language Features Matrix

This section provides a comprehensive, systematic catalog of all language features in LFE. It serves as a reference for understanding what constructs are available, their syntax, semantics, and usage patterns.

## Overview

LFE is a **Lisp-2** with:

- **Separate namespaces** for functions and variables
- **S-expression syntax** (homoiconic)
- **Erlang semantics** (immutable data, process-oriented, pattern matching)
- **Full Erlang interoperability** (zero-overhead FFI)
- **Powerful macro system** (compile-time code transformation)
- **Multiple paradigm support** (functional, concurrent, actor-based)

**Language Characteristics**:

| Aspect | Description |
|--------|-------------|
| **Type System** | Dynamically typed with optional Dialyzer specs |
| **Evaluation** | Eager/strict evaluation (call-by-value) |
| **Scoping** | Lexical scoping with environment-based bindings |
| **Closures** | Full closure support with lambda lifting |
| **Tail Calls** | Tail call optimization (via Erlang/BEAM) |
| **Concurrency** | Actor model (Erlang processes) |
| **Persistence** | All data structures immutable |
| **Memory Model** | Garbage collected, per-process heaps |

## Core Forms (23)

```
quote cons car cdr list tuple binary map
lambda match-lambda
let let* letrec let-function letrec-function let-macro
progn if case receive try catch
call funcall function
```

## Built-in Macros (50+)

```
defmodule defun defmacro defrecord defstruct
cond when unless do
let* flet fletrec flet*
lc bc qlc
caar cadr cdar cddr ... (c*r family)
list* != === !==
```

## Data Types (13)

```
atom integer float string binary boolean
list tuple map record struct
function pid port reference
```

## Operators (30+)

```
Arithmetic: + - * / div rem abs
Comparison: == /= =:= =/= < > =< >=
Boolean: not and or andalso orelse xor
Bitwise: band bor bxor bnot bsl bsr
```

### LFE vs Common Lisp

| Feature | LFE | Common Lisp | Notes |
|---------|-----|-------------|-------|
| Namespaces | Lisp-2 | Lisp-2 | Separate function/variable namespaces |
| Evaluation | Eager | Eager | Call-by-value |
| Typing | Dynamic | Dynamic | Runtime type checking |
| Macros | Unhygienic | Unhygienic | Both support quasiquotation |
| CLOS | No | Yes | LFE uses Erlang behaviors instead |
| Tail calls | Yes (BEAM) | Yes | Proper TCO |
| Concurrency | Actors (processes) | Threads | Different models |
| Mutability | Immutable | Mutable | Erlang semantics |

### LFE vs Scheme

| Feature | LFE | Scheme | Notes |
|---------|-----|--------|-------|
| Namespaces | Lisp-2 | Lisp-1 | LFE has separate namespaces |
| Evaluation | Eager | Eager | Both call-by-value |
| Macros | Unhygienic | Hygienic (syntax-rules) | LFE has unhygienic macros |
| Continuations | No | Yes (call/cc) | BEAM doesn't support |
| Tail calls | Yes | Yes | Both have proper TCO |
| Numbers | Erlang types | Scheme numbers | Different numeric tower |

### LFE vs Clojure

| Feature | LFE | Clojure | Notes |
|---------|-----|---------|-------|
| Platform | BEAM/Erlang | JVM | Different VMs |
| Namespaces | Lisp-2 | Lisp-1 | Different conventions |
| Data structures | Erlang types | Persistent collections | Both immutable |
| Concurrency | Actors | Agents/Atoms/STM | Different models |
| Macros | Traditional | Hygienic-ish | Clojure has syntax-quote |
| Typing | Dynamic | Dynamic | Both with optional specs |
| Performance | BEAM-level | JVM-level | Different characteristics |
