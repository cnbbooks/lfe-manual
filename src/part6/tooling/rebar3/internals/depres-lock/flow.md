# Execution Flow

```mermaid
graph TD
    A[Start: install_deps provider] --> B[Get current profiles]
    B --> C[Get project apps]
    C --> D[Check upgrade flag]
    D --> E[deps_per_profile]

    E --> E1[For each profile]
    E1 --> E2[Get parsed deps for profile]
    E2 --> E3{More profiles?}
    E3 -->|Yes| E1
    E3 -->|No| F[Get locks from state]

    F --> G[handle_profile_level]
    G --> G1{Deps remaining?}
    G1 -->|No| H[find_cycles]
    G1 -->|Yes| G2[For each dep at current level]

    G2 --> G3{Dep is top-level app?}
    G3 -->|Yes| G4[Skip this dep]
    G3 -->|No| G5{Dep already seen?}

    G4 --> G6{More deps at level?}
    G5 -->|Yes| G7[Check version conflict]
    G5 -->|No| G8[maybe_lock]

    G7 --> G9{Same source?}
    G9 -->|Yes| G6
    G9 -->|No| G10[Warn about skip]
    G10 --> G6

    G8 --> G11{In default profile?}
    G11 -->|Yes| G12[Add to lock list]
    G11 -->|No| G13[Skip locking]

    G12 --> G14[maybe_fetch]
    G13 --> G14

    G14 --> G15[handle_dep]
    G15 --> G16[Parse dep's deps]
    G16 --> G17[Add to next level]
    G17 --> G6

    G6 -->|Yes| G2
    G6 -->|No| G18[Process next level]
    G18 --> G1

    H --> H1[Build digraph of all apps]
    H1 --> H2[Add vertices for each app]
    H2 --> H3[Add edges for dependencies]
    H3 --> H4[digraph_utils:topsort]

    H4 --> H5{Acyclic?}
    H5 -->|Yes| H6[Return sorted list]
    H5 -->|No| H7[Find strongly connected components]
    H7 --> H8[Return cycles error]

    H6 --> I[cull_compile]
    I --> I1[Remove project apps from sorted list]
    I1 --> I2[Drop deps that don't need compile]
    I2 --> J[Update state: deps_to_build]

    J --> K[lock provider]
    K --> K1{Running in default profile?}
    K1 -->|No| L[Skip locking]
    K1 -->|Yes| K2[build_locks]

    K2 --> K3[Get all locked deps from state]
    K3 --> K4[For each dep not checkout]
    K4 --> K5[Create lock tuple]
    K5 --> K6[Sort locks by name]
    K6 --> K7[maybe_write_lock_file]

    K7 --> K8{Locks changed?}
    K8 -->|Yes| K9[Write rebar.lock]
    K8 -->|No| K10[Check format changed]
    K10 --> K11{Format different?}
    K11 -->|Yes| K9
    K11 -->|No| L

    K9 --> L[End: Dependencies resolved and locked]

    style A fill:#e1f5ff
    style L fill:#e1ffe1
```

## Detailed Steps

1. **Profile-Based Dependency Collection** (`deps_per_profile/3`)
   - Get active profiles (e.g., `[default]`, `[default, test]`)
   - For each profile, get parsed dependencies
   - Load lock file data for consistency

2. **Level-Order Dependency Traversal** (`handle_profile_level/7`)
   - Process dependencies level by level (breadth-first)
   - Level 0: Direct dependencies from `rebar.config`
   - Level 1: Dependencies of level 0 dependencies
   - Continue until no more dependencies found

3. **Dependency Processing** (`update_dep/9`)
   - For each dependency:
     - Check if already seen (to avoid duplicates)
     - Check if it's a top-level app (skip if so)
     - Add to lock list if in default profile
     - Fetch the dependency (or verify it exists)
     - Parse the dependency's own dependencies
     - Add transitive deps to next level

4. **Lock Management** (`maybe_lock/5`)
   - Only lock dependencies in default profile
   - Skip checkout dependencies
   - Track dependency level (depth in tree)
   - Replace existing locks if new one is shallower

5. **Cycle Detection** (`find_cycles/1`)
   - Build directed graph of all applications
   - Vertices: application names
   - Edges: application → dependency relationships
   - Use `digraph_utils:topsort/1` for topological sort
   - If sort fails, find strongly connected components (cycles)

6. **Compilation Order** (`cull_compile/2`)
   - Start with topologically sorted dependency list
   - Remove project applications (they compile separately)
   - Drop dependencies that don't need compilation (already built)
   - Return final list for `deps_to_build`

7. **Lock File Writing** (`lock` provider)
   - Only runs in default profile
   - Build lock entries from state
   - Each lock: `{Name, Source, Level}`
   - Sort locks alphabetically
   - Write to `rebar.lock` if changed
   - Preserve lock file format version
