# Error Conditions

## Missing Required Applications

**Condition**: Required Erlang application fails to start (e.g., `crypto`, `ssl`)

**Detection**: `application:start/1` returns `{error, Reason}`

**Error Message**:

```
Rebar dependency <app> could not be loaded for reason <reason>
```

**Recovery**: None; rebar3 exits with code 1

---

## Unsupported OTP Version

**Condition**: OTP version is below `minimum_otp_vsn`

**Detection**: `rebar_utils:check_min_otp_version/2`

**Error Message**:

```
ERROR: OTP release <version> does not match required regex <regex>
```

**Recovery**: None; must upgrade OTP

---

## Blacklisted OTP Version

**Condition**: OTP version matches `blacklisted_otp_vsns`

**Detection**: `rebar_utils:check_blacklisted_otp_versions/1`

**Error Message**:

```
ERROR: OTP release <version> matches blacklisted version <regex>
```

**Recovery**: None; must change OTP version

---

## Invalid Configuration Format

**Condition**: `rebar.config` contains invalid Erlang terms

**Detection**: Parse error in `rebar_config:consult_file/1`

**Error Message**:

```
ERROR: Error reading config file <file>: <parse error>
```

**Recovery**: None; must fix `rebar.config`

---

## Invalid Profile Configuration

**Condition**: Profile configuration is not a list

**Detection**: `rebar_state:apply_profiles/2`

**Error**:

```erlang
{profile_not_list, Profile, Other}
```

**Error Message**:

```
Profile config must be a list but for profile '<profile>' config given as: <other>
```

**Recovery**: None; must fix profile configuration

---

## Newer Lock File Version

**Condition**: `rebar.lock` has a newer version than supported

**Detection**: Version check in `rebar_config:consult_lock_file/1`

**Warning Message**:

```
Rebar3 detected a lock file from a newer version. It will be loaded in compatibility mode,
but important information may be missing or lost. It is recommended to upgrade Rebar3.
```

**Recovery**: Loads in compatibility mode; may lose some information

---

## Command Not Found

**Condition**: Requested command doesn't match any provider

**Detection**: `rebar_core:process_namespace/2`

**Error Message**:

```
Command <command> not found
```

**Recovery**: None; check command spelling or available commands
