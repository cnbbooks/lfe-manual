# Functions & API Calls

## rebar_state API

**Creation**:

- `new/0,1,2,3`: Create state with configuration

**Configuration**:

- `get/2,3`: Get configuration value
- `set/3`: Set configuration value
- `opts/1,2`: Get/set options dict
- `default/1,2`: Get/set default options

**Applications**:

- `project_apps/1,2`: Get/set project applications
- `all_deps/1,2`: Get/set all dependencies
- `deps_to_build/1,2`: Get/set dependencies to compile
- `update_all_deps/2`: Update dependency list

**Compilers**:

- `compilers/1,2`: Get/set compiler list
- `prepend_compilers/2`: Add compilers at start
- `append_compilers/2`: Add compilers at end

**Providers**:

- `providers/1,2`: Get/set providers
- `add_provider/2`: Register new provider

**Profiles**:

- `current_profiles/1,2`: Get/set active profiles
- `apply_profiles/2`: Apply profile configurations

**Paths**:

- `dir/1,2`: Get/set project directory
- `code_paths/2,3`: Get/set code paths

## rebar_app_info API

**Creation**:

- `new/0,1,2,3,4,5`: Create app info with varying detail

**Basic Fields**:

- `name/1,2`: Get/set application name
- `vsn/1,2`: Get/set version
- `dir/1,2`: Get/set source directory
- `out_dir/1,2`: Get/set output directory
- `ebin_dir/1,2`: Get/set ebin directory

**Application Files**:

- `app_file/1,2`: Get/set `.app` path
- `app_file_src/1,2`: Get/set `.app.src` path
- `app_file_src_script/1,2`: Get/set `.app.src.script` path

**Dependencies**:

- `deps/1,2`: Get/set build dependencies
- `applications/1,2`: Get/set runtime dependencies
- `dep_level/1,2`: Get/set dependency depth

**Configuration**:

- `opts/1,2`: Get/set options
- `get/2,3`: Get config value
- `set/3`: Set config value
- `apply_profiles/2`: Apply profiles

**Status**:

- `valid/1,2`: Get/set validity
- `is_available/1,2`: Get/set availability
- `is_checkout/1,2`: Get/set checkout status
- `is_lock/1,2`: Get/set lock status

**Type**:

- `project_type/1,2`: Get/set project type (rebar3, mix, etc.)
