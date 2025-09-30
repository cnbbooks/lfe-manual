# Compilation

The LFE REPL provides several functions for compiling source files and loading the resulting modules.

## Compiling LFE Files

Use the `c` function to compile and load LFE modules:

```lisp
lfe> (c "my-module.lfe")
#(module my-module)
```

This function:

* Compiles the LFE source file
* Loads the resulting `.beam` file into the current session
* Makes the module's exported functions available for use
* Places the compiled `.beam` file in the current working directory

You can also provide compilation options:

```lisp
lfe> (c "my-module.lfe" '(debug_info export_all))
#(module my-module)
```

Common compilation options include:

* `debug_info` - Include debugging information
* `export_all` - Export all functions (useful for development)
* `warn_unused_vars` - Warn about unused variables

## Compiling Erlang Files

To compile Erlang source files from the LFE REPL, use the `ec` function:

```lisp
lfe> (ec "utility.erl")
#(module utility)
```

This allows you to work with existing Erlang code from within your LFE development session.

## Working with Compiled Modules

Once a module is compiled and loaded, you can call its functions using the standard module:function syntax:

```lisp
lfe> (my-module:some-function arg1 arg2)
result
```

You can also check which modules are currently loaded:

```lisp
lfe> (m)
;; Lists all loaded modules
lfe> (m 'my-module)
;; Shows information about a specific module
```

## Cleanup

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Take care
  </h4>
  <p class="mb-0">
    When using the <code>(c)</code> command in the REPL, compiled <code>.beam</code> files are placed in the current working directory, not in a proper <code>ebin</code> directory. This can lead to serious development issues if not properly managed.
  </p>
</div>

If you compile a module in the REPL and forget to clean up the resulting `.beam` file, you may encounter mysterious issues later, naming continuing to see old behaviour from the module compiled in your current working directory instead of the .beam file most recently updated.
