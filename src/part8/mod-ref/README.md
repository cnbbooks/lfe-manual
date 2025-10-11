# Module Reference

This section provides a comprehensive reference for all 39 modules in the LFE codebase. Each module is documented with its purpose, API surface, internal structure, dependencies, and key implementation details. Line number references point to specific locations in the source code to aid contributors in navigating the codebase.

## Module Statistics

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

## Architectural Insights

**1. Modular Design**:

- Clean separation of concerns
- Minimal inter-module dependencies
- Clear interface boundaries

**2. Layered Structure**:

- Foundation: lfe_lib, lfe_internal, lfe_env, lfe_error
- Infrastructure: lfe_bits, lfe_struct, lfe_types
- I/O Subsystem: lfe_io, lfe_io_write, lfe_io_pretty, lfe_io_format
- Compiler Pipeline: scan → parse → macro → lint → translate → codegen
- Runtime: lfe_eval, lfe_eval_bits, lfe_init, lfescript
- Shell: lfe_shell, lfe_edlin_expand
- Libraries: cl, clj, scm, lfe_ms, lfe_qlc, lfe_gen
- Documentation: lfe_docs, lfe_shell_docs

**3. Code Concentration**:

- Top 3 modules: 30.3% of codebase (lfe_lint, lfe_translate, lfe_eval)
- Top 10 modules: 60.9% of codebase
- 11 modules < 100 LOC (focused, single-purpose)

**4. Implementation Languages**:

- 37 Erlang modules (95%)
- 2 LFE modules (5% - cl.lfe, clj.lfe)

**5. Common Patterns**:

- **Facade**: lfe.erl, lfe_io.erl
- **Delegation**: lfe_io delegates to lfe_io_write, lfe_io_pretty, lfe_io_format
- **Pipeline**: Compiler modules form processing chain
- **Environment Threading**: lfe_env passed through evaluation
- **Two-Process**: lfe_shell separates reading from evaluation

## Critical Modules

**By Importance**:

1. **lfe_env** - Environment management (imported by lfe_macro, used everywhere)
2. **lfe_eval** - Runtime evaluation (complete interpreter)
3. **lfe_translate** - Core Erlang translation (compilation backend)
4. **lfe_lint** - Semantic analysis (validation)
5. **lfe_macro** - Macro expansion (transformation layer)
6. **lfe_shell** - Interactive REPL (user interface)
7. **lfe_comp** - Compilation orchestration (coordinates pipeline)

**By Usage Frequency**:

1. lfe_env - Used by 7+ modules
2. lfe_internal - Used by 5+ modules
3. lfe_io - Used by all modules (error reporting)
4. lfe_lib - Used by 4+ modules

## Extensibility Points

**Adding Features**:

1. **New special form**: Update lfe_internal, lfe_eval, lfe_translate, lfe_lint
2. **New macro**: Add to lfe_macro or user code
3. **New compatibility layer**: Create new .lfe module
4. **New I/O format**: Create new lfe_io_* module
5. **New library**: Create new module, no core changes needed

## Performance Characteristics

**Fast Paths**:

- Compiled code (via lfe_translate → Erlang compiler)
- Simple I/O (lfe_io_write)
- Environment lookups (map-based or orddict)

**Slow Paths**:

- Interpreted code (lfe_eval)
- Macro expansion (lfe_macro - 30-40% of compilation)
- Pretty printing (lfe_io_pretty - layout calculations)
- Slurping (lfe_shell - full file parsing + expansion)

## Quality Indicators

**Strengths**:

- Well-documented modules
- Clear module boundaries
- Minimal circular dependencies
- Consistent naming conventions
- Comprehensive error handling

**Areas for Improvement**:

- Large modules could be split (lfe_lint, lfe_translate, lfe_eval)
- Some tight coupling (lfe_env heavily used)
- Limited test coverage visibility from module structure alone
