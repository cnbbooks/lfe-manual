# Key Design Decisions

## Two-Stage Compilation Strategy

**Decision**: Compile LFE → Core Erlang → BEAM, rather than LFE → BEAM directly.

**Rationale**:

1. **Leverage existing infrastructure**: Reuse Erlang's battle-tested optimizer and code generator
2. **Pattern compilation**: Exploit Core Erlang's efficient case statement representation
3. **Tool compatibility**: Dialyzer, Cover, Xref all understand Core Erlang
4. **Maintenance reduction**: No need to maintain BEAM code generation
5. **Optimization inheritance**: Automatically benefit from Erlang compiler improvements

**Trade-off**: Extra translation layer adds compile-time overhead, but enables perfect interop and reduces implementation complexity dramatically.

## S-Expression Syntax (Homoiconicity)

**Decision**: Use pure S-expressions rather than inventing syntax.

**Impact**:

- **Parser is trivial**: 284 LOC for complete parser vs. 1000s in typical languages
- **Macros are natural**: Code-as-data means macro manipulation is just list processing
- **No AST library needed**: S-expressions *are* the AST
- **Metaprogramming is first-class**: Quasiquotation and manipulation are built-in
- **Tooling simplification**: Pretty-printing, code formatting, analysis all easier

**Cost**: Parentheses-heavy syntax unfamiliar to non-Lisp programmers (mitigated by Lisp's large existing community).

## Macro System Design

**Decision**: Provide both procedural macros (lambda/match-lambda) and pattern-based macros (syntax-rules).

**Capabilities**:

- **Procedural macros**: Full Turing-complete transformation (receive environment as `$ENV`)
- **Pattern macros**: Declarative Scheme-style syntax-rules with ellipsis
- **Hygiene mechanism**: Manual via gensym (not automatic like Scheme)
- **Compile-time eval**: `eval-when-compile` for computation during macro expansion
- **Macro export**: Cross-module macro sharing

**50+ built-in macros** provide Common Lisp-style convenience (defun, defmodule, cond, etc.).

## Dual Execution Philosophy

**Decision**: Support both compilation *and* interpretation.

**Compiled mode:**

- Production use
- Maximum performance
- Full optimization
- Dialyzer integration

**Interpreted mode:**

- REPL development
- Dynamic code loading (slurp)
- Compile-time evaluation
- Interactive debugging

Both paths share the same frontend (scanner, parser, macro expander), ensuring semantic consistency.

## Environment as Core Abstraction

**Decision**: Use a single unified environment representation (`lfe_env.erl`) across all subsystems.

**Usage**:

- **Compiler**: Tracks macro definitions, record definitions during expansion/linting
- **Runtime**: Tracks variable/function bindings during evaluation
- **Shell**: Maintains persistent state across REPL interactions

**Implementation**: Immutable record with maps/orddict for bindings, enabling functional threading through compilation/evaluation passes.

## Pattern Matching Everywhere

**Decision**: Make pattern matching a first-class, pervasive feature.

**Contexts**:

- Function clauses (`match-lambda`)
- Let bindings (`let`)
- Case expressions (`case`)
- Message receiving (`receive`)
- Comprehension generators (`lc`, `bc`)
- Record/struct access
- Match specifications (ETS/trace)

**Consistency**: Same pattern language across all contexts (with context-appropriate restrictions for guards).

## Three-Way Compatibility

**Decision**: Provide separate compatibility modules for Common Lisp, Clojure, and Scheme rather than trying to "be" any of them.

**Modules**:

- **cl.lfe** (767 LOC): Common Lisp functions, symbol properties, sequences
- **clj.lfe** (842 LOC): Clojure threading macros, predicates, lazy sequences
- **scm.erl** (276 LOC): Scheme syntax-rules, begin, define

**Philosophy**: Explicit namespacing (`(: cl member ...)`) makes semantics clear. No hidden behavior changes. Programmers from any Lisp background can be productive immediately.

## Records vs. Structs Duality

**Decision**: Support both Erlang-style records (tuples) and Elixir-style structs (maps).

**Records**:

- Tuple-based: `{record_name, field1, field2, ...}`
- Fixed size, positional access
- Erlang/OTP compatible
- Used in gen_server callbacks, OTP behaviors

**Structs**:

- Map-based: `#{__struct__ => module, field => value, ...}`
- Flexible size, named access
- Modern developer ergonomics
- Elixir-inspired

**Rationale**: Provide the right tool for each job—legacy compatibility vs. modern ergonomics.
