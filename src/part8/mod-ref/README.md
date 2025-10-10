# Module Reference

This section provides a comprehensive reference for all 39 modules in the LFE codebase. Each module is documented with its purpose, API surface, internal structure, dependencies, and key implementation details. Line number references point to specific locations in the source code to aid contributors in navigating the codebase.

The LFE codebase consists of **39 modules** totaling **20,272 lines of code**:

- **37 Erlang modules** (.erl files)
- **2 LFE modules** (.lfe files: `cl.lfe`, `clj.lfe`)

**Module Distribution by Category:**

| Category | Module Count | LOC | % of Codebase |
|----------|--------------|-----|---------------|
| Compiler | 13 | 8,011 | 39.5% |
| Runtime | 4 | 2,595 | 12.8% |
| Library | 9 | 3,404 | 16.8% |
| I/O | 4 | 1,601 | 7.9% |
| Shell | 2 | 1,402 | 6.9% |
| Support Utilities | 6 | 1,174 | 5.8% |
| Compatibility | 3 | 1,885 | 9.3% |
| Documentation | 2 | 372 | 1.8% |
| Integration | 1 | 51 | 0.3% |

**Top 10 Modules by Size** (comprising 60.9% of the codebase):

1. `lfe_lint.erl` - 2,532 LOC (semantic analysis)
2. `lfe_translate.erl` - 2,182 LOC (AST translation)
3. `lfe_eval.erl` - 2,004 LOC (runtime evaluator)
4. `lfe_macro.erl` - 1,432 LOC (macro system)
5. `lfe_shell.erl` - 1,188 LOC (REPL)
6. `lfe_scan.erl` - 897 LOC (lexer)
7. `clj.lfe` - 842 LOC (Clojure compatibility)
8. `lfe_codelift.erl` - 785 LOC (lambda lifting)
9. `cl.lfe` - 767 LOC (Common Lisp compatibility)
10. `lfe_comp.erl` - 711 LOC (compilation orchestrator)
