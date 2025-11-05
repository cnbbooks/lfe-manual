# Edge Cases

## No `rebar.config` File

**Behavior**: rebar3 uses built-in defaults

**Impact**:

- No dependencies
- Default compiler options
- Standard directory layout expected

---

## Empty `rebar.config`

**Behavior**: Treated same as missing file

**Impact**: Uses all defaults

---

## `rebar.config.script`

**Behavior**: If `rebar.config.script` exists, it's evaluated as Erlang code

**Purpose**: Dynamic configuration generation

**Example**:

```erlang
CONFIG1 = case os:getenv("ENV") of
    "prod" -> [{erl_opts, [no_debug_info]}];
    _ -> []
end,
CONFIG1 ++ CONFIG.
```

---

## Global Config Without Local Config

**Behavior**: Global config provides base configuration

**Use Case**: Setting up a new project with global plugins available

---

## Multiple Profiles Applied

**Behavior**: Profiles merge in order, later values override earlier

**Example**:

```
rebar3 as test,custom compile
```

Applies profiles: `default` → `test` → `custom`

---

## `REBAR_PROFILE` Environment Variable with `as` Command

**Behavior**: Environment variable overrides are replaced by `as` profiles

**Recommendation**: Use `as` command instead of environment variable
