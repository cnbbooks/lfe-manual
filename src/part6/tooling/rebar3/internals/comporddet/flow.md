# Execution Flow

```mermaid
graph TD
    A[Start: Determine Compilation Order] --> B{Context?}
    B -->|Dependencies| C[find_cycles in install_deps]
    B -->|Project Apps| D[compile_order in compile provider]

    C --> E[rebar_digraph:compile_order/1]
    D --> E

    E --> E1[Create new digraph]
    E1 --> E2[For each application]

    E2 --> E3[Get app name]
    E3 --> E4[Get app dependencies]
    E4 --> E5[all_apps_deps: Union of applications + deps]

    E5 --> E6[Add vertex for app name]
    E6 --> E7[For each dependency]
    E7 --> E8[Add vertex for dep name if not exists]
    E8 --> E9[Add edge: app -> dep]

    E9 --> E10{More deps?}
    E10 -->|Yes| E7
    E10 -->|No| E11{More apps?}
    E11 -->|Yes| E2
    E11 -->|No| F[digraph_utils:topsort/1]

    F --> F1{Sort successful?}
    F1 -->|Yes| G[Reverse the sorted list]
    F1 -->|No| H[Check if acyclic]

    G --> I[Map names back to app_info records]
    I --> J[Delete digraph]
    J --> K[Return sorted apps]

    H --> H1{Is acyclic?}
    H1 -->|Yes| H2[Return no_sort error]
    H1 -->|No| H3[Find strongly connected components]

    H3 --> H4[Filter components: length > 1]
    H4 --> H5[Sort cycles]
    H5 --> H6[Return cycles error]

    H2 --> J
    H6 --> J

    K --> L{Context?}
    L -->|Dependencies| M[cull_compile: Filter deps]
    L -->|Project Apps| N[Use sorted list directly]

    M --> M1[Remove project apps from list]
    M1 --> M2[Drop deps that don't need compile]
    M2 --> O[Return final compile order]

    N --> O

    style A fill:#e1f5ff
    style O fill:#e1ffe1
```

## Detailed Steps

1. **Graph Construction** (`compile_order/1`)
   - Create empty directed graph with `digraph:new/0`
   - For each application in the input list:
     - Extract application name
     - Get all dependencies (see `all_apps_deps/1`)
     - Add vertex for application name
     - For each dependency: add vertex and edge

2. **Dependency Collection** (`all_apps_deps/1`)
   - Get `applications` list from `.app` file (runtime dependencies)
   - Get `deps` list from `rebar.config` (build-time dependencies)
   - Convert both to binaries
   - Sort and merge (union of both lists)
   - This ensures all relevant dependencies are considered

3. **Topological Sort** (`digraph_utils:topsort/1`)
   - Standard Erlang digraph utility performs topological sort
   - Returns list in dependency order (dependencies first)
   - Returns `false` if graph contains cycles

4. **Cycle Detection** (on sort failure)
   - Check if graph is acyclic with `digraph_utils:is_acyclic/1`
   - If acyclic but sort failed: return `no_sort` error (rare edge case)
   - If not acyclic: find strongly connected components
   - Strongly connected components with length > 1 are cycles
   - Sort and return cycle information

5. **List Reversal**
   - `topsort` returns dependencies first
   - Reverse the list for compilation (dependencies last)
   - **Why reversed**: Original sort gives evaluation order; we want build order

6. **Name to AppInfo Mapping** (`names_to_apps/2`)
   - Sorted list contains application names (atoms/binaries)
   - Map back to full `rebar_app_info:t()` records
   - Preserve sort order
   - Skip any apps not found in original list

7. **Graph Cleanup**
   - Delete digraph with `digraph:delete/1`
   - Free memory
   - Return final sorted list

8. **Compilation Filtering** (`cull_compile/2` - for dependencies only)
   - Remove project applications from sorted dependency list
   - Drop dependencies that don't need compilation
   - Determine "needs compile" based on:
     - Checkout dependencies: always need compile
     - Source dependencies (Git, Hg): need compile
     - Package dependencies (Hex): may not need compile (pre-built)
