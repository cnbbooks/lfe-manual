# Loading

LFE provides several ways to load pre-compiled modules into your REPL session.

## Using `l` (Load Module)

The `l` function loads or reloads a specific module:

```lisp
lfe> (l 'my-module)
#(module my-module)
```

This is useful when:

* You've recompiled a module and want to reload it
* You want to load a module that exists but isn't currently loaded
* You're working with hot code reloading during development

## Using `code:ensure_loaded`

For more control over the loading process, you can use Erlang's code server directly:

```lisp
lfe> (code:ensure_loaded 'my-module)
#(module my-module)
```

This function:

* Loads the module if it's not already loaded
* Does nothing if the module is already loaded
* Returns error information if the module can't be found

## Checking Module Status

You can check what modules are currently loaded and get information about them:

```lisp
;; List all loaded modules
lfe> (m)

;; Get information about a specific module
lfe> (m 'lists)

;; Check if a module is loaded
lfe> (code:is_loaded 'my-module)
#(file "/path/to/my-module.beam")
```

### Hot Code Reloading

During development, you can reload modules without restarting the REPL:

```lisp
;; Edit and recompile your module file
lfe> (c "my-module.lfe")
#(module my-module)

;; The module is automatically reloaded with new code
lfe> (my-module:updated-function)
```
