# High-Level Architecture

## System Layers

LFE is organized into nine architectural layers, from foundational utilities to user-facing applications:

```
┌─────────────────────────────────────────────────────────┐
│  Layer 9: Applications & CLI Tools                      │
│  lfec, lfescript, lfe (REPL)                            │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 8: Shell & Interactive Environment               │
│  lfe_shell, lfe_edlin_expand                            │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 7: Documentation System                          │
│  lfe_docs, lfe_shell_docs                               │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 6: Compilation Pipeline                          │
│  lfe_comp (orchestrator)                                │
│  lfe_scan → lfe_parse → lfe_macro → lfe_lint →          │
│  lfe_codegen → lfe_translate                            │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 5: Runtime & Evaluation                          │
│  lfe_eval, lfe_env                                      │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 4: Standard Library                              │
│  lfe_io, lfe_lib, lfe_gen, lfe_types                    │
│  lfe_bits, lfe_ms, lfe_qlc                              │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 3: Compatibility Layers                          │
│  cl (Common Lisp), clj (Clojure), scm (Scheme)          │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 2: Language Internals                            │
│  lfe_internal (core forms, special forms, intrinsics)   │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  Layer 1: Foundation                                    │
│  lfe.hrl, lfe_comp.hrl, lfe_macro.hrl (shared defs)     │
└─────────────────────────────────────────────────────────┘
```

## Compilation Pipeline Architecture

The compilation pipeline is a linear, acyclic flow with distinct phases:

```
Source File (.lfe)
    ↓
┌──────────────────┐
│  lfe_scan.erl    │  Lexical Analysis: Text → Tokens
│  (897 LOC)       │  Handles: strings, atoms, numbers, symbols, sigils
└──────────────────┘
    ↓ Tokens
┌──────────────────┐
│  lfe_parse.erl   │  Syntactic Analysis: Tokens → S-expressions
│  (284 LOC)       │  LL(1) parser, trivial due to S-expr syntax
└──────────────────┘
    ↓ S-expressions
┌──────────────────┐
│  lfe_macro.erl   │  Macro Expansion: Transform AST
│  (1,432 LOC)     │  Recursive expansion, hygiene, quasiquotation
└──────────────────┘
    ↓ Expanded Forms
┌──────────────────┐
│  lfe_lint.erl    │  Semantic Analysis: Validate & Check
│  (2,532 LOC)     │  Variable binding, type checking, pattern validation
└──────────────────┘
    ↓ Validated Forms
┌──────────────────┐
│  lfe_docs.erl    │  Documentation Extraction: EEP-48 chunks
│  (362 LOC)       │  Extract docstrings, specs, metadata
└──────────────────┘
    ↓ Forms + Docs
┌──────────────────┐
│  lfe_codegen.erl │  Code Generation: Lambda lifting, AST building
│  (499 LOC)       │  Prepare Erlang Abstract Format
└──────────────────┘
    ↓ Erlang AST (pre-translation)
┌──────────────────┐
│ lfe_translate.erl│  AST Translation: LFE Forms → Erlang Abstract Format
│  (2,182 LOC)     │  Pattern/guard/expression translation
└──────────────────┘
    ↓ Complete Erlang AST
┌──────────────────┐
│ compile:forms/2  │  Erlang Compiler: AST → BEAM
│ (Erlang stdlib)  │  Optimizations, code generation, BEAM emission
└──────────────────┘
    ↓
BEAM Bytecode (.beam)
```

**Key metrics:**

- **Total compiler code**: ~8,000 LOC (39.5% of codebase)
- **Largest module**: `lfe_lint.erl` (2,532 LOC) - semantic analysis
- **Second largest**: `lfe_translate.erl` (2,182 LOC) - AST translation
- **Macro system**: `lfe_macro.erl` (1,432 LOC) - the heart of LFE's power
- **Parser simplicity**: `lfe_parse.erl` (284 LOC) - S-expressions make parsing trivial

## Runtime System Architecture

LFE provides a complete interpreter alongside its compiler, enabling dual execution modes:

```
┌─────────────────────────────────────────────────────────┐
│  Compiled Execution Path                                │
│  Source → Compile → BEAM → Execute on VM                │
│  (Fast, optimized, production)                          │
└─────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────┐
│  Interpreted Execution Path                             │
│  S-expr → Macro Expand → lfe_eval → Value               │
│  (Interactive, REPL, eval-when-compile)                 │
└─────────────────────────────────────────────────────────┘
```

**Runtime components:**

- **lfe_eval.erl** (2,004 LOC): Complete interpreter with all special forms
- **lfe_env.erl** (252 LOC): Environment management (bindings, scoping)
- **lfe_shell.erl** (1,188 LOC): Interactive REPL with job control
- **lfe_edlin_expand.erl** (175 LOC): Tab completion system

**Shell architecture** uses a two-process model:

```
┌──────────────────┐         ┌─────────────────────┐
│  Shell Process   │ <-----> │  Evaluator Process  │
│  - I/O handling  │  msgs   │  - Expression eval  │
│  - History       │         │  - Pattern matching │
│  - State mgmt    │         │  - Isolated crashes │
└──────────────────┘         └─────────────────────┘
```

This isolation ensures evaluator crashes don't kill the shell, enabling robust interactive development.
