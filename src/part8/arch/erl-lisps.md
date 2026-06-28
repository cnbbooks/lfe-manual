# Relationship to Erlang and Other Lisps

## Relationship to Erlang

LFE is **not**:

- A syntax overlay (it has different semantics via macros)
- An FFI to Erlang (there is no foreign function interface)
- A separate runtime (it *is* the BEAM VM)
- An Erlang dialect (it's a different language that compiles to same target)

LFE **is**:

- **Semantically equivalent** at the bytecode level
- **Perfectly interoperable** with zero overhead
- **OTP-native** (all behaviors, supervision, applications work)
- **Tool-compatible** (Dialyzer, Observer, Debugger, etc. work seamlessly)

**Code equivalence example:**

```lisp
;; LFE
(defun factorial (n)
  (factorial n 1))

(defun factorial (0 acc) acc)
(defun factorial (n acc) (when (> n 0))
  (factorial (- n 1) (* n acc)))
```

```erlang
%% Equivalent Erlang
factorial(N) -> factorial(N, 1).

factorial(0, Acc) -> Acc;
factorial(N, Acc) when N > 0 ->
    factorial(N - 1, N * Acc).
```

Both compile to **identical BEAM bytecode**.

## Relationship to Common Lisp

LFE **borrows from Common Lisp**:

- Lisp-2 namespace separation (functions and variables distinct)
- Macro style (lambda-based, not pattern-based)
- Quote/backquote/comma syntax
- Many function names (car, cdr, mapcar, etc.)

LFE **differs from Common Lisp**:

- No CLOS (use Erlang records/behaviors instead)
- No condition system (use Erlang try/catch/exit)
- No mutable data structures (Erlang immutability)
- Pattern matching is primary (vs. destructuring-bind)
- Processes instead of threads
- Message passing instead of shared state

**Compatibility**: The `cl` module provides 60+ CL functions with correct semantics (including `()` = false).

## Relationship to Clojure

LFE **borrows from Clojure**:

- Threading macros (`->`, `->>`, `as->`, `some->`, etc.)
- Conditional macros (`if-let`, `when-let`, `condp`)
- Rich set of predicates
- Lazy sequence concepts
- Functional composition style

LFE **differs from Clojure**:

- No persistent data structures (Erlang uses copying immutability)
- No STM (Erlang uses actors/message passing)
- Pattern matching is primary (vs. destructuring)
- Lisp-2 (Clojure is Lisp-1)
- S-expressions (Clojure has reader macros for vectors/maps)

**Compatibility**: The `clj` module provides threading macros and predicates with Clojure-like semantics.

## Relationship to Scheme

LFE **borrows from Scheme**:

- Syntax-rules (macro-by-example system)
- Hygienic macro concepts
- Simple, elegant core
- Lexical scoping only

LFE **differs from Scheme**:

- Lisp-2 (Scheme is Lisp-1)
- Eager evaluation (Scheme encourages laziness)
- Pattern matching pervasive
- No continuations (Erlang has processes instead)
- Macro expansion is explicit, not automatic hygiene

**Compatibility**: The `scm` module provides full syntax-rules implementation with ellipsis support.

## Unique Position in Language Ecosystem

LFE occupies a unique niche:

```
Lisp Family          BEAM VM Family
    │                      │
    │                      │
    ├─ Common Lisp         ├─ Erlang
    ├─ Scheme              ├─ Elixir
    ├─ Clojure (JVM)       │
    │                      │
    └─────────┬────────────┘
              │
              └─ LFE (intersection)
                 - Lisp syntax & macros
                 - BEAM semantics & concurrency
                 - Three-dialect compatibility
                 - Zero-overhead interop
```

**No other language provides:**

- Lisp metaprogramming on BEAM VM
- Pattern matching as primary paradigm in Lisp
- Actor model concurrency in Lisp
- OTP supervision/behaviors in Lisp
- Erlang tool compatibility in Lisp
