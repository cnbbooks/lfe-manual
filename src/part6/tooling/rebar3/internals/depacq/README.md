# Dependency Acquisition

## Purpose

The dependency acquisition stage fetches dependencies from their respective sources (Hex package registry, Git repositories, Mercurial repositories, local checkouts), verifies their integrity, extracts them to the appropriate locations, and prepares them for compilation. This stage ensures all required dependencies are available locally before compilation begins.

## When It Executes

This stage executes as part of the dependency resolution process in [Dependency Resolution & Locking](/part6/tooling/rebar3/internals/depres-lock/), specifically within the `update_unseen_dep/9` function via the `maybe_fetch/5` call. It runs during:

- `install_deps` provider execution
- First compilation when dependencies aren't cached
- When dependencies need updating
- After modifying `rebar.config` dependencies

## Prerequisites

- Dependencies resolved with specific versions/refs
- Source information available (Hex, Git URL, etc.)
- Network access (unless offline mode or dependencies cached)
- Git/Mercurial tools installed (for VCS dependencies)

## Outputs

- Dependencies downloaded to `_build/PROFILE/lib/DEPNAME/`
- Dependency configurations parsed and validated
- Application info updated with actual dependency structure
- Dependencies ready for compilation
