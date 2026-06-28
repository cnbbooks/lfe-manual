# What is LFE?

Lisp Flavoured Erlang (LFE) is a production-ready Lisp-2 dialect that compiles to BEAM bytecode, running natively on the Erlang VM. It is not a toy language, a proof-of-concept, or a syntax overlay—it is a sophisticated, self-hosting compiler and runtime system that achieves true zero-overhead interoperability with Erlang and OTP while providing the full power of Lisp's homoiconic macro system.

## The Core Achievement

LFE accomplishes something remarkable: **complete semantic equivalence with Erlang at the bytecode level** while providing **complete syntactic and metaprogramming capabilities of Lisp**. This is achieved through a two-stage compilation strategy:

```
LFE Source → Macro Expansion → Core Erlang → BEAM Bytecode
```

Unlike foreign function interfaces (FFI) or runtime interop layers, LFE code *is* Erlang code. A function compiled from LFE generates identical BEAM bytecode to an equivalent function written in Erlang. There is no marshaling, no type conversion, no performance penalty. LFE functions are Erlang functions, LFE processes are Erlang processes, and LFE modules are Erlang modules.

## Language Classification

**LFE is:**

- **A Lisp-2**: Functions and variables inhabit separate namespaces (like Common Lisp, unlike Scheme)
- **Homoiconic**: Code is data; S-expressions are the native AST
- **Eagerly evaluated**: Call-by-value semantics with left-to-right argument evaluation
- **Lexically scoped**: All bindings use lexical scope (no dynamic scoping)
- **Macro-powered**: Compile-time metaprogramming with full code-as-data manipulation
- **Pattern-matching native**: Pattern matching is pervasive across all binding forms
- **Concurrency-oriented**: Full access to Erlang's actor model and OTP framework
- **Functionally pure** (within Erlang's constraints): Immutable data, pure functions, side effects explicit

**LFE provides:**

- Complete Erlang/OTP compatibility (processes, supervision, behaviors, gen_server, etc.)
- Self-hosting compilation (LFE compiler written in LFE)
- Dual execution modes (compiled to BEAM or interpreted in REPL)
- Three Lisp dialect compatibility layers (Common Lisp, Clojure, Scheme)
- Full Dialyzer support for static type analysis
- Comprehensive standard library
- Production-grade tooling (REPL, compiler, build integration, documentation system)
