# rebar3 Internals - A Developer's Reference

## Introduction

The rebar3 compilation system is a sophisticated, multi-stage build system designed to compile Erlang/OTP applications and their dependencies efficiently. The system follows a provider-based plugin architecture, uses directed acyclic graphs (DAGs) for dependency tracking, and supports incremental and parallel compilation.

This document provides a bird's-eye view of the entire compilation process, outlining the major stages and their relationships. For detailed technical information about each stage, see the individual stage documents referenced throughout.

## High-Level Architecture

The rebar3 compilation chain is built on several architectural principles:

1. **Provider-Based Execution**: All functionality is implemented as providers that declare dependencies and execution order
2. **State Management**: A central state object flows through all stages, accumulating configuration and results
3. **Graph-Based Dependencies**: File and application dependencies are tracked using directed acyclic graphs
4. **Incremental Compilation**: Only changed files and their dependents are recompiled
5. **Parallel Compilation**: Independent files can be compiled concurrently
6. **Extensibility**: Custom compilers and hooks allow modification of the build process

## Major Compilation Stages

The compilation process can be divided into the following major stages:

1. Initialization and Configuration Loading
2. Dependency Resolution and Locking
3. Dependency Acquisition
4. Application Discovery
5. Compilation Order Determination
6. Dependency Compilation
7. Project Application Compilation
8. Single Application Compilation
9. Individual File Compilation
10. Build Verification and Completion

## Key Dependencies

- Dependency resolution must precede acquisition
- Applications must be discovered before determining compilation order
- Dependencies must compile before project applications
- Compilation order is critical for correctness
- Verification ensures build integrity
