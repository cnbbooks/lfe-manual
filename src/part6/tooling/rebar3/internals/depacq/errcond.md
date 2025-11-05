# Error Conditions

## Offline Mode Without Cache

**Condition**: Dependency not cached and offline mode enabled

**Error Format**:

```erlang
{?MODULE, offline}
```

**Error Message**:

```
Cannot fetch dependency in offline mode
```

**Recovery**:

- Disable offline mode
- Or pre-cache dependencies

---

## Network Failure

**Condition**: Cannot connect to remote server

**Causes**:

- No internet connection
- Server down
- Firewall blocking

**Error Format**:

```erlang
{fetch_fail, Source}
```

**Error Message**:

```
Failed to fetch and copy dep: {git, "https://github.com/user/repo.git", {tag, "1.0.0"}}
```

**Recovery**: Check network and retry

---

## Bad Registry Checksum

**Condition**: Downloaded package checksum doesn't match registry

**Error Format**:

```erlang
{bad_registry_checksum, Name, Version, Expected, Found}
```

**Error Message**:

```
The checksum for package at jsx-3.1.0 (ABC123) does not match the checksum
expected from the registry (DEF456). Run `rebar3 do unlock jsx, update` and then try again.
```

**Causes**:

- Corrupted download
- Tampered package
- Cache corruption

**Recovery**:

```bash
rebar3 do unlock jsx, update
```

---

## Git Clone Failure

**Condition**: Git clone command fails

**Common Causes**:

- Invalid URL
- Authentication failure
- Repository doesn't exist
- Network timeout

**Error Message**:

```
fatal: repository 'https://github.com/user/repo.git' not found
```

**Recovery**:

- Verify URL
- Check authentication
- Verify repository exists

---

## Git Version Too Old

**Condition**: Git < 1.8.5 when locking

**Error Message**:

```
Can't lock git dependency: git version must be 1.8.5 or higher.
```

**Recovery**: Upgrade Git

---

## Dependency Application Not Found

**Condition**: Downloaded dependency doesn't contain valid OTP application

**Error Format**:

```erlang
{dep_app_not_found, Name}
```

**Error Message**:

```
Dependency failure: source for my_dep does not contain a recognizable project and can not be built
```

**Causes**:

- No `.app.src` file
- Invalid application structure
- Wrong directory layout

**Recovery**:

- Check dependency is valid OTP app
- Verify repository URL is correct

---

## Hex Package Not Found

**Condition**: Hex returns 404 for package

**Error**: Handled in resolution stage, not acquisition
