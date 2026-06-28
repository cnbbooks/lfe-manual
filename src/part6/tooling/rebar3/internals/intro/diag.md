# Diagrams

## Flow Diagram

```mermaid
graph TD
    A[Start: rebar3 compile] --> B[Initialization and Configuration Loading]
    B --> C[Dependency Resolution and Locking]
    C --> D[Dependency Acquisition]
    D --> E[Application Discovery]
    E --> F[Compilation Order Determination]
    F --> G{deps_only flag?}

    G -->|Yes| H[Dependency Compilation Only]
    G -->|No| I[Dependency Compilation]

    H --> H1[For Each Dependency]
    H1 --> H2[Single Application Compilation]
    H2 --> H3{More deps?}
    H3 -->|Yes| H1
    H3 -->|No| Z[End]

    I --> I1[For Each Dependency]
    I1 --> I2[Single Application Compilation]
    I2 --> I3{More deps?}
    I3 -->|Yes| I1
    I3 -->|No| J[Project Application Compilation]

    J --> J1[For Each Project App]
    J1 --> J2[Single Application Compilation]
    J2 --> J3{More apps?}
    J3 -->|Yes| J1
    J3 -->|No| K[Root Extra Dirs Compilation]

    K --> L[Build Verification and Completion]
    L --> Z[End]

    style A fill:#e1f5ff
    style Z fill:#e1f5ff
    style H2 fill:#fff4e1
    style I2 fill:#fff4e1
    style J2 fill:#fff4e1
```

## Single Application Compilation Flow

```mermaid
graph TD
    SA[Single Application Compilation] --> SA1[Application Preparation]
    SA1 --> SA2[Pre-Compilation Hooks]
    SA2 --> SA3[Compiler Preparation Hooks]
    SA3 --> SA4[Source Compilation Loop]

    SA4 --> SC1[Leex Compiler: .xrl → .erl]
    SC1 --> SC2[Yecc Compiler: .yrl → .erl]
    SC2 --> SC3[MIB Compiler: .mib → .bin + .hrl]
    SC3 --> SC4[Erlang Compiler: .erl → .beam]
    SC4 --> SC5[Custom Compilers]

    SC5 --> SA5[Post-Compiler Hooks]
    SA5 --> SA6[App File Preparation Hooks]
    SA6 --> SA7[Application File Generation]
    SA7 --> SA8[App File Finalization Hooks]
    SA8 --> SA9[Compilation Finalization]
    SA9 --> SAE[Application Compiled]

    style SA fill:#fff4e1
    style SAE fill:#e1ffe1
```

## Individual Compiler Execution Flow

```mermaid
graph TD
    C[Compiler Execution] --> C1[Load or Initialize DAG]
    C1 --> C2[Get Compiler Context]
    C2 --> C3[Scan Source Directories]
    C3 --> C4[Prune Deleted Files from DAG]
    C4 --> C5[Analyze Source Dependencies]

    C5 --> C6[For Each Source File]
    C6 --> C7[Parse Dependencies]
    C7 --> C8[Add to DAG]
    C8 --> C9{More sources?}
    C9 -->|Yes| C6
    C9 -->|No| C10[Propagate Timestamps]

    C10 --> C11[Determine Needed Files]
    C11 --> C12{Files to compile?}
    C12 -->|No| C20[Save DAG and Exit]
    C12 -->|Yes| C13[Separate First Files]

    C13 --> C14[Compile Parse Transforms]
    C14 --> C15[Split Sequential/Parallel]
    C15 --> C16[Compile Sequential Files]
    C16 --> C17[Compile Parallel Files]

    C17 --> C18[Store Artifact Metadata]
    C18 --> C19[Update DAG Timestamps]
    C19 --> C20
    C20 --> CE[Compiler Done]

    style C fill:#e1f5ff
    style CE fill:#e1ffe1
```

## Stage Dependencies

The following diagram shows dependencies between major stages:

```mermaid
graph LR
    Init[Initialization] --> DepResolve[Dependency Resolution]
    DepResolve --> DepAcq[Dependency Acquisition]
    DepAcq --> AppDisc[Application Discovery]
    AppDisc --> CompOrder[Compilation Order]
    CompOrder --> DepComp[Dependency Compilation]
    CompOrder --> ProjComp[Project Compilation]
    DepComp --> ProjComp
    ProjComp --> Verify[Build Verification]

    style Init fill:#e1f5ff
    style Verify fill:#e1ffe1
```
