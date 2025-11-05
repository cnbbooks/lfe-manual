# State Modification

## Fields Set During Initialization

| Field | When Set | Value | Purpose |
|-------|----------|-------|---------|
| `dir` | `new/0,1` | `rebar_dir:get_cwd()` | Project root directory |
| `opts` | `new/1` | Parsed `rebar.config` | Configuration options |
| `default` | `new/1` | Copy of `opts` | Original configuration before profile application |
| `escript_path` | `init_config/0` | Path to rebar3 escript | For extracting embedded resources |
| `lock` | `new/3` | Parsed `rebar.lock` | Locked dependency versions |
| `current_profiles` | `apply_profiles/2` | `[default]` or specified | Active configuration profiles |
| `compilers` | `run_aux/2` | `[rebar_compiler_xrl, ...]` | Registered compiler modules |
| `resources` | `run_aux/2` | `[rebar_git_resource, ...]` | Resource modules for fetching deps |
| `providers` | `create_logic_providers/2` | List of provider records | Available commands |
| `command_args` | `run_aux/2` | Remaining CLI args | Arguments to pass to command |

## Configuration Keys in `opts`

Common configuration keys stored in the `opts` dictionary:

- `deps`: List of dependencies
- `plugins`: List of plugins
- `project_plugins`: List of project-scoped plugins
- `profiles`: Profile-specific configuration
- `erl_opts`: Erlang compiler options
- `src_dirs`: Source directories
- `extra_src_dirs`: Additional source directories
- `minimum_otp_vsn`: Minimum OTP version required
- `base_dir`: Build output directory
- `artifacts`: List of expected artifacts
