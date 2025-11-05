# Application File Generation

## Purpose

Application file generation transforms `.app.src` (application resource source) files into `.app` (application resource) files by substituting variables, adding module lists, determining version numbers, ensuring required fields exist, and writing the final application specification to the `ebin/` directory. This stage makes applications ready for deployment and release generation.

## When It Executes

This stage executes within the `compile/4` function in [Source File Compilation](/part6/tooling/rebar3/internals/srcflcomp), specifically in the `compile_app_files/3` sub-stage:

**Sequence**:

1. Source files compiled → `.beam` files created
2. `prepare_app_file`: Pre-hooks executed
3. **`compile_app_files`: This stage**
4. `finalize_app_file`: Post-hooks executed

## Prerequisites

- `.app.src` or `.app.src.script` file exists
- Source files compiled to `.beam` files in `ebin/`
- Version information available (from config, git, or other sources)
- Application info record initialized

## Outputs

- `.app` file in `ebin/` directory
- Complete application specification with:
  - Accurate module list
  - Resolved version number
  - All required fields (`registered`, `description`)
  - Variable substitutions applied
