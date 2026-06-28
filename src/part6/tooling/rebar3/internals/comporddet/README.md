# Compilation Order Determination

## Purpose

The compilation order determination stage takes the list of resolved dependencies and project applications and determines the correct sequence in which they must be compiled. This stage uses graph-based topological sorting to ensure that each application is compiled after all of its dependencies, preventing compilation errors due to missing dependencies. This applies to both dependency compilation and project application compilation separately.

## When It Executes

This stage executes within the `compile` provider in two distinct contexts:

1. **Dependency Compilation**: During [Dependency Resolution & Locking](/part6/tooling/rebar3/internals/depres-lock/), the `find_cycles/1` function determines the order for dependencies
2. **Project App Compilation**: In `rebar_prv_compile:do/1`, the `compile_order/1` function determines order for project applications

## Prerequisites

- Dependencies resolved with complete dependency information
- Project applications discovered with metadata
- Application dependencies extracted (both runtime `applications` and build-time `deps`)
- No circular dependencies (or detection ready to report them)

## Outputs

- Topologically sorted list of applications
- Dependencies listed before dependents
- Error if circular dependencies detected
- Order ready for sequential compilation
