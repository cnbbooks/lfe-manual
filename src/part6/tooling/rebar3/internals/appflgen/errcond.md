# Error Conditions

## Missing .app.src File

**Condition**: No `.app.src` or `.app.src.script` found

**Error**: `{missing_app_file, Filename}`

**Message**: `"App file is missing: src/my_app.app.src"`

**Recovery**: Create `.app.src` file

---

## Invalid Application Name

**Condition**: Name in file doesn't match filename

**Example**:

```erlang
% File: ebin/my_app.app
% Content:
{application, wrong_name, [...]}.
```

**Error**: `{invalid_name, File, AppName}`

**Message**: `"Invalid ebin/my_app.app: name of application (wrong_name) must match filename."`

**Recovery**: Fix name in `.app.src`

---

## Missing Module

**Condition**: Module listed in `.app` but no `.beam` file exists

**Error**: From `validate_application_info/2`

**Example**: `.app` lists `my_module` but `ebin/my_module.beam` missing

**Common Causes**:

- Compilation failed for that module
- Module manually added to `.app.src`
- Typo in module name

**Recovery**: Ensure module compiles successfully

---

## Parse Error in .app.src

**Condition**: Syntax error in `.app.src`

**Error**: `{file_read, AppName, ".app.src", Reason}`

**Example**: Missing comma, unmatched bracket

**Recovery**: Fix syntax

---

## Git VCS Command Failure

**Condition**: `{vsn, git}` but git command fails

**Possible Issues**:

- Not a git repository
- No tags exist
- Git not installed

**Fallback**: May use "0" or error
