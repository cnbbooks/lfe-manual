# Initialization & Configuration

## Purpose

The initialization stage prepares the rebar3 runtime environment by loading configuration files, setting up logging, initializing the state structure, loading plugins, and registering providers. This stage establishes the foundation for all subsequent compilation and build operations.

## When It Executes

This is the **first stage** in the rebar3 execution chain. It runs immediately when:

- `rebar3 compile` (or any command) is invoked from the command line
- `rebar3:run/2` is called from the Erlang API

## Prerequisites

- Erlang/OTP runtime is available
- Current working directory is accessible
- Required Erlang applications (crypto, ssl, inets) can be started

## Outputs

- Initialized `rebar_state:t()` record containing:
  - Loaded configuration from `rebar.config`
  - Merged global configuration (if `~/.config/rebar3/rebar.config` exists)
  - Applied profiles
  - Registered providers
  - Installed plugins
  - Code paths for the build
  - Lock file data
