# Stage 7: AST Translation (lfe_translate.erl)

**Purpose**: Translate between LFE forms and Erlang Abstract Format.

**Module**: `lfe_translate.erl` (2,182 LOC - **second largest**) - Location: `src/lfe_translate.erl`

**Bidirectional translation**:

```erlang
%% LFE → Erlang
to_expr(LFEForm, Line) -> ErlangAST
to_expr(LFEForm, Line, {Imports, Aliases}) -> ErlangAST

%% Erlang → LFE
from_expr(ErlangAST) -> LFEForm
from_expr(ErlangAST, Line) -> LFEForm
```

**Key translation examples**:

```erlang
%% Data constructors
[quote, E]           → {atom, Line, E}  (for atoms)
[cons, H, T]         → {cons, Line, H', T'}
[list | Es]          → nested cons / {nil, Line}
[tuple | Es]         → {tuple, Line, [E1', ...]}
[binary | Segs]      → {bin, Line, Segments}
[map | KVs]          → {map, Line, Assocs}

%% Functions
[lambda, Args, Body]     → {'fun', Line, {clauses, [Clause]}}
[match-lambda | Clauses] → {'fun', Line, {clauses, Clauses}}

%% Control flow
[if, Test, Then, Else]   → {'case', Line, Test, [...]}
[case, Expr, Clauses]    → {'case', Line, Expr, Clauses}
[receive | Clauses]      → {'receive', Line, Clauses}
[try, Expr, ...]         → {'try', Line, ...}

%% Function calls
[funcall, F | Args]      → {call, Line, F', Args'}
[call, M, F | Args]      → {call, Line, {remote, Line, M', F'}, Args'}
[F | Args]               → {call, Line, {atom, Line, F}, Args'}
```

**Pattern translation**:

Patterns use similar syntax but different semantics:

- Variables become `{var, Line, Var}`
- `[= Pat1 Pat2]` becomes alias patterns
- Maps become `{map_pat, Line, [...]}`

**Guard translation**:

Guards have **restricted expressions**:

- BIFs only (no user functions)
- Comparisons: `>`, `>=`, `<`, `=<`, `==`, `=:=`, `/=`, `=/=`
- Arithmetic: `+`, `-`, `*`, `/`, `div`, `rem`, `band`, `bor`, `bxor`, `bnot`, `bsl`, `bsr`
- Boolean: `and`, `or`, `xor`, `not`, `andalso`, `orelse`
- Type tests: `is_atom`, `is_integer`, `is_list`, etc.

**Import/alias resolution**:

During translation, the `{Imports, Aliases}` context resolves:

- Imported function calls → remote calls
- Module aliases → full module names

Example:

```lisp
;; With import: (from lists (map 2))
(map fun list)  →  {call, Line, {remote, Line, {atom, Line, lists}, {atom, Line, map}}, [Fun', List']}
```
