# Files

So far, everything we've looked at in the REPL involves typing (or pasting) code. When wanting to use the REPL to experiment with more complicated code, there's a better, time-honoured way: files. There are several ways you can use file-based code in the REPL:

* evaluation
* compilation
* including
* loading

Each of these is covered in more detail below.

## Evaluation: `slurp` and `unslurp`

LFE provides a unique and powerful way to evaluate file contents directly in the REPL through the `slurp` and `unslurp` commands. This feature allows you to temporarily import function and macro definitions from files into your current REPL session.

### Using `slurp`

The `slurp` function reads an LFE source file and makes all functions and macros defined in that file available directly in the shell, without requiring module prefixes:

```lisp
lfe> (slurp "examples/my-functions.lfe")
#(ok -no-mod-)
lfe> $ENV
;; Shows all the new function and macro definitions from the file
```

Key characteristics of `slurp`:

* Only one file can be slurped at a time
* Slurping a new file automatically removes all data from the previously slurped file
* Functions and macros become available without module prefixes
* The code is evaluated in the current REPL environment
* Variable bindings from the file are added to your current session

### Using `unslurp`

The `unslurp` command reverts the REPL back to the state before the last slurp, removing all function and macro definitions that were imported:

```lisp
lfe> (unslurp)
ok
lfe> $ENV
;; Back to the original environment state
```

This is particularly useful when experimenting with different versions of functions or when you want to clean up your REPL environment.

### Practical Example

Let's say you have a file called `math-utils.lfe`:

```lisp
(defun double (x)
  (* x 2))

(defun square (x)
  (* x x))

(defmacro when-positive (x body)
  `(if (> ,x 0) ,body 'not-positive))
```

After slurping this file:

```lisp
lfe> (slurp "math-utils.lfe")
#(ok -no-mod-)
lfe> (double 21)
42
lfe> (square 6)
36
lfe> (when-positive 5 'yes)
yes
lfe> (when-positive -1 'yes)
not-positive
```

## Compiling Files

The LFE REPL provides several functions for compiling source files and loading the resulting modules.

### Compiling LFE Files

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

### Compiling Erlang Files

To compile Erlang source files from the LFE REPL, use the `ec` function:

```lisp
lfe> (ec "utility.erl")
#(module utility)
```

This allows you to work with existing Erlang code from within your LFE development session.

### Working with Compiled Modules

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

## Using `include-lib` and `include-file`

LFE supports including header files and library files, which is essential for larger projects and when working with Erlang/OTP libraries.

### `include-lib`

The `include-lib` directive allows you to include files from installed OTP applications or other libraries:

```lisp
(include-lib "kernel/include/file.hrl")
(include-lib "stdlib/include/qlc.hrl")
```

This searches for the include file in the standard library locations and makes the definitions available in your code.

### `include-file`

The `include-file` directive includes files using relative or absolute paths:

```lisp
(include-file "local-definitions.lfe")
(include-file "../shared/common.lfe")
(include-file "/absolute/path/to/file.lfe")
```

### Include File Content

Include files typically contain:

* Record definitions
* Macro definitions
* Constant definitions
* Type specifications

Example include file (`records.lfe`):

```lisp
(defrecord person
  name
  age
  email)

(defmacro debug (msg)
  `(io:format "DEBUG: ~p~n" (list ,msg)))
```

After including this file, you can use the record and macro definitions in your code:

```lisp
lfe> (include-file "records.lfe")
debug // In LFE, when including a file in the REPL, the last function defined in the file is printed to stdout

lfe> (make-person name "Robert" age 54 email "robert@lfe.io")
#(person "Robert" 54 "robert@lfe.io")
lfe>
```

```lisp
lfe> (debug "oops")
DEBUG: "oops"
ok
```

## Module Loading

LFE provides several ways to load pre-compiled modules into your REPL session.

### Using `l` (Load Module)

The `l` function loads or reloads a specific module:

```lisp
lfe> (l 'my-module)
#(module my-module)
```

This is useful when:

* You've recompiled a module and want to reload it
* You want to load a module that exists but isn't currently loaded
* You're working with hot code reloading during development

### Using `code:ensure_loaded`

For more control over the loading process, you can use Erlang's code server directly:

```lisp
lfe> (: code ensure_loaded 'my-module)
#(module my-module)
```

This function:

* Loads the module if it's not already loaded
* Does nothing if the module is already loaded
* Returns error information if the module can't be found

### Checking Module Status

You can check what modules are currently loaded and get information about them:

```lisp
;; List all loaded modules
lfe> (m)

;; Get information about a specific module
lfe> (m 'lists)

;; Check if a module is loaded
lfe> (: code is_loaded 'my-module)
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

## Practical Development Workflow

Here's a typical workflow combining these file operations:

1. **Start with experimentation** using `slurp` for quick testing:

   ```lisp
   lfe> (slurp "experimental-functions.lfe")
   ```

2. **Convert to proper modules** when code stabilizes:

   ```lisp
   lfe> (unslurp)
   lfe> (c "my-module.lfe")
   ```

3. **Use includes** for shared definitions:

   ```lisp
   ;; In your module file
   (include-file "common-records.lfe")
   ```

4. **Load additional modules** as needed:

   ```lisp
   lfe> (l 'helper-module)
   lfe> (: code ensure_loaded 'third-party-lib)
   ```

5. **Iterate with hot reloading**:

   ```lisp
   ;; Edit file, then:
   lfe> (c "my-module.lfe")
   ;; Test immediately
   ```

This file-based approach to REPL development provides the flexibility to move between experimental, interactive coding and structured, modular development as your project evolves.

## Cleanup: Managing Compiled Files

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Cleanup
  </h4>
  <p class="mb-0">
    When using the <code>(c)</code> command in the REPL, compiled <code>.beam</code> files are placed in the current working directory, not in a proper <code>ebin</code> directory. This can lead to serious development issues if not properly managed.
  </p>
</div>

### The Problem

If you compile a module in the REPL and forget to clean up the resulting `.beam` file, you may encounter mysterious issues later:

```lisp
;; You're experimenting in /tmp
lfe> (pwd)
"/tmp"
lfe> (c "my-module.lfe")  ; Creates /tmp/my-module.beam
#(module my-module)
```

Later, when you're working on your real project and make changes to the actual `my-module.lfe` file in your project's `src/` directory, the old `.beam` file in `/tmp` might take precedence in the module loading order, causing your new changes to be completely ignored.

This can lead to hours or even days of debugging confusion where your code changes seem to have no effect!

### The Solution: Always Clean Up

After experimenting with compiled modules, always clean up the `.beam` files:

```lisp
;; Check what files are in your current directory
lfe> (ls)
;; Look for .beam files

;; Remove experimental .beam files
lfe> (: file delete "my-module.beam")
ok

;; Or from your system shell
$ rm *.beam
```

### Best Practices for File Management

1. **Use a dedicated experiment directory**:

   ```bash
   mkdir ~/lfe-experiments
   cd ~/lfe-experiments
   lfe  # Start LFE REPL here
   ```

2. **Clean up regularly**:

   ```lisp
   ;; At the end of each REPL session
   lfe> (ls)  ; Check for .beam files
   lfe> (: file delete "experimental-module.beam")
   ```

3. **Use `slurp` instead of `c` for experimentation**:

   ```lisp
   ;; Preferred for experiments - no .beam files created
   lfe> (slurp "my-experimental-code.lfe")

   ;; Instead of
   lfe> (c "my-experimental-code.lfe")  ; Creates .beam file
   ```

4. **Check your working directory**:

   ```lisp
   ;; Always be aware of where you are
   lfe> (pwd)
   lfe> (ls)  ; Look for unexpected .beam files
   ```

### Emergency Recovery

If you suspect old `.beam` files are interfering with your development:

1. **Find and remove stray `.beam` files**:

   ```bash
   # Search for .beam files in unexpected locations
   $ find ~/workspace -name "*.beam" -not -path "*/ebin/*" -not -path "*/_build/*"

   # Remove them carefully
   $ find ~/workspace -name "*.beam" -not -path "*/ebin/*" -not -path "*/_build/*" -delete
   ```

2. **Clear the code cache**:

   ```lisp
   lfe> (: code purge 'problematic-module)
   true
   lfe> (: code delete 'problematic-module)
   true
   lfe> (: code ensure_loaded 'problematic-module)
   #(module problematic-module)
   ```

3. **Restart your development environment** to ensure clean module loading.

### Summary

* ✅ **Use `slurp` for experimentation** - no cleanup needed
* ✅ **Use dedicated experiment directories** when using `(c)`
* ✅ **Always clean up `.beam` files** after REPL experiments
* ✅ **Check `(pwd)` and `(ls)`** regularly during REPL sessions
* ❌ **Never leave experimental `.beam` files** in your working directories

Remember: A few seconds of cleanup can save you hours of debugging mysterious behavior!
