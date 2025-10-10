# Compilation Pipeline

The LFE compiler is a multi-stage pipeline where each stage performs a distinct transformation. The pipeline is **strictly linear and acyclic**—there are no back-edges or iterative refinement loops. This design simplifies reasoning about compilation and makes the pipeline highly composable.

```mermaid
graph TB
    A[Source Text<br/>.lfe file] -->|String| B[lfe_scan.erl<br/>Lexical Analysis]
    B -->|Token Stream| C[lfe_parse.erl<br/>Syntactic Analysis]
    C -->|S-expressions| D[lfe_comp.erl<br/>File Splitting]
    D -->|Module Forms| E[lfe_macro.erl<br/>Macro Expansion]
    E -->|Expanded Forms| F[lfe_lint.erl<br/>Semantic Analysis]
    F -->|Validated Forms| G[lfe_docs.erl<br/>Doc Extraction]
    G -->|Forms + Docs| H[lfe_codegen.erl<br/>Code Generation]
    H -->|Erlang AST| I[lfe_translate.erl<br/>AST Translation]
    I -->|Complete Erlang AST| J[compile:forms/2<br/>Erlang Compiler]
    J -->|BEAM Bytecode| K[.beam File]

    style A fill:#e1f5ff
    style K fill:#ffe1e1
    style E fill:#f0e1ff
    style F fill:#e1ffe1
    style I fill:#ffe1f5
```

**Key characteristics:**

- **One-way flow**: Each stage consumes input from previous stage, produces output for next
- **Explicit state threading**: Compilation state (`#comp{}` record) threaded through all stages
- **Error accumulation**: Errors collected but don't stop compilation (report all errors at once)
- **Incremental optimization**: Each stage can be stopped early for debugging/analysis
