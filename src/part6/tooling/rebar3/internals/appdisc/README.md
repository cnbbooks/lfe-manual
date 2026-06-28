# Application Discovery

## Purpose

The application discovery stage scans project directories to locate all OTP applications, parses their application resource files (`.app`, `.app.src`, `.app.src.script`, or `mix.exs`), extracts application metadata, validates application structure, and prepares application information records for compilation. This stage determines which applications are part of the project and which are dependencies.

## When It Executes

This stage executes early in the build process via the `app_discovery` provider, which is a dependency of `install_deps`. It runs:

- After [Initialization & Configuration](/part6/tooling/rebar3/internals/init-cfg/)
- Before [Dependency Resolution & Locking](/part6/tooling/rebar3/internals/depres-lock/)
- On every `rebar3 compile` invocation
- Whenever project structure changes

## Prerequisites

- State initialized with configuration
- Project root directory determined
- Configuration specifying `lib_dirs` and `src_dirs`
- File system accessible

## Outputs

- List of project applications (`rebar_state:project_apps/1`)
- Parsed dependencies per profile (`{parsed_deps, Profile}` in state)
- Application info records for each discovered app
- Determination of top-level app vs umbrella project structure
