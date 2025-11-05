# Key Concepts

## Provider

A provider is a unit of functionality in rebar3. Each provider:

- Has a unique name and namespace
- Declares dependencies on other providers
- Implements initialization and execution functions
- Can be extended or overridden by plugins

The compile provider depends on the lock provider, ensuring dependencies are resolved before compilation begins.

## State

The rebar state is a record that flows through all stages, containing:

- Configuration options from `rebar.config`
- Lists of applications (project apps, dependencies, plugins)
- Code paths for different contexts
- Active profiles
- Registered providers and compilers
- Current execution context

## Application Info

Each application (project or dependency) has an associated app info record containing:

- Application name and version
- Source and output directories
- Path to `.app.src` and `.app` files
- Application-specific configuration options
- Dependency list
- Profile information

## Directed Acyclic Graph (DAG)

Each compiler maintains a DAG to track file dependencies:

- **Vertices**: Source files, header files, and artifacts with timestamps
- **Edges**: Dependency relationships
- **Purpose**: Incremental compilation, determining compilation order
- **Persistence**: Saved to disk between builds
- **Invalidation**: Recalculated if compiler version or critical options change

## Compiler

A compiler is a module implementing the `rebar_compiler` behavior:

- Defines what source files it handles (extensions, directories)
- Analyzes dependencies between files
- Determines compilation order
- Executes compilation for individual files
- Tracks generated artifacts

## Hooks

Hooks allow execution of custom code at specific points:

- **Provider hooks**: Run other rebar3 providers
- **Shell hooks**: Execute shell commands
- **Pre/post hooks**: Run before or after specific stages
- **Platform-specific**: Conditional execution based on OS

## Profiles

Profiles allow configuration variations:

- Default profile: `default`
- Common profiles: `test`, `prod`
- Custom profiles: User-defined
- Profile stacking: Configuration merges from default + active profiles
- Separate build outputs: `_build/PROFILE/`

## Incremental Compilation

Optimization that avoids unnecessary recompilation:

- Tracks file modification timestamps
- Propagates timestamps through dependency chains
- Recompiles only changed files and their dependents
- Detects compiler option changes
- Stores metadata with artifacts

## Parallel Compilation

Performance optimization for independent files:

- Files with no interdependencies compiled concurrently
- Worker pool manages parallel tasks
- Parse transforms and dependent files compiled sequentially
- Configurable parallelism level
