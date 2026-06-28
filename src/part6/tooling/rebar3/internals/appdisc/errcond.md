# Error Conditions

## Multiple App Files

**Condition**: More than one `.app.src` file in same directory

**Error Format**:

```erlang
{multiple_app_files, Files}
```

**Error Message**:

```
Multiple app files found in one app dir: my_app.app.src and other.app.src
```

**Recovery**: Remove duplicate files

---

## Invalid App File

**Condition**: Application resource file has syntax errors

**Error Format**:

```erlang
{invalid_app_file, File, Reason}
```

**Error Message**:

```
Invalid app file apps/my_app/src/my_app.app.src at line 5: syntax error before: '}'
```

**Recovery**: Fix syntax in app file

---

## Missing App File

**Condition**: Application directory has no valid resource file

**Behavior**: Application skipped, not an error

**Log**: Debug message only

---

## OTP Version Mismatch

**Condition**: Application requires different OTP version

**Error**: Thrown from `rebar_app_info:verify_otp_vsn/1`

**Configuration**: Use `minimum_otp_vsn` in app's `rebar.config`
