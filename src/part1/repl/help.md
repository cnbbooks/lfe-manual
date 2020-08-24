# `(help)`

As you gain familiarity with the LFE REPL, one of the most useful and convenient references will be the summary of functions, commands, and variablese that come with the LFE REPL.

To see these, simple call the `help` or `h` function:

```lisp
lfe> (help)
```

That will result in the following being displyed to your terminal:

```lisp
LFE shell built-in functions

(c file)       -- compile and load code in <file>
(cd dir)       -- change working directory to <dir>
(clear)        -- clear the REPL output
(doc mod)      -- documentation of a module
(doc mod mac)  -- documentation of a macro
(doc m f a)    -- documentation of a function
(ec file)      -- compile and load code in erlang <file>
(ep expr)      -- print a term in erlang form
(epp expr)     -- pretty print a term in erlang form
(exit)         -- quit - an alias for (q)
(flush)        -- flush any messages sent to the shell
(h)            -- an alias for (help)
(help)         -- help info
(i)            -- information about the system
(i pids)       -- information about a list of pids
(l module)     -- load or reload <module>
(ls)           -- list files in the current directory
(ls dir)       -- list files in directory <dir>
(m)            -- which modules are loaded
(m mod)        -- information about module <mod>
(p expr)       -- print a term
(pp expr)      -- pretty print a term
(pid x y z)    -- convert <x>, <y> and <z> to a pid
(pwd)          -- print working directory
(q)            -- quit - shorthand for init:stop/0
(regs)         -- information about registered processes

LFE shell built-in commands

(reset-environment)             -- reset the environment to its initial state
(run file)                      -- execute all the shell commands in a <file>
(set pattern expr)
(set pattern (when guard) expr) -- evaluate <expr> and match the result with
                                   pattern binding
(slurp file)                    -- slurp in a LFE source <file> and makes
                                   everything available in the shell
(unslurp)                       -- revert back to the state before the last
                                   slurp

LFE shell built-in variables

+/++/+++      -- the tree previous expressions
*/**/***      -- the values of the previous expressions
-             -- the current expression output
$ENV          -- the current LFE environment

ok
```

Most of those are documented in stdlib reference for their [Erlang counterparts](http://erlang.org/doc/man/c.html), so be sure to reference that information for details on many of the above.

Those not covered in that Erlang reference manual, or those that are different in their LFE versionsm, include:

* Built-in Functions
  * Compilation functions
  * LFE code documentation
  * Printing and pretty-printing
* LFE shell built-in commands
* The "LFE shell built-in variables"

## Built-in Functions

### Compilation

If you view the Erlang reference manual documentation for compiling files in the shell, you will see differences from what is show in the LFE help text. In particular, `(c)` is for compiling LFE modules and `(ec)` needs to be used for compiling Erlang source files.

In both cases, the resulting `.beam` files are compiled to the current working directory and not to an `ebin` directory.

### Documentation

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    Incorrect Help Text
  </h4>
  <p class="mb-0">
    The help text for accessing LFE module, macro, and function documentation is not correct: it lists three functions of arity 1, 2, and 3. There is only one function with one arity. Module, macros, function name, and arity are all provided as a single argument of an unquoted atom.
  </p>
  <p class="mb-0">
    There is an open ticket tracking this issue here: <a href="https://github.com/rvirding/lfe/issues/401">LFE issue #401</a>.
  </p>
</div>

You may access the documentation for LFE modules, macros, and functions in the REPL via the `doc` function. For instance, the Common Lisp compatibility module's documentation:

```lisp
lfe> (doc cl)
;; ____________________________________________________________
;; cl
;;
;; LFE Common Lisp interface library.
;;
;; ok
```

That module's `cond` macro documentation:

```lisp
lfe> (doc cl:cond)
;; ____________________________________________________________
;; cond
;; args
;; CL compatible cond macro.
;;
;; ok
```

That module's `pairlis/2` function documentation:

```lisp
lfe> (doc cl:pairlis/2)
;; ____________________________________________________________
;; pairlis/2
;; keys values
;; Make an alist from pairs of keys values.
;;
;; ok
```

# Printing Data

LFE provides some nice convenience functions for displaying data structions in the REPL. Let's say we had a data structure defined thusly:

```lisp
lfe> (set data `(#(foo bar baz) #(quux quuz) #(corge grault garply)
lfe> #(plugh xyzzy) #(flurb nirf) #(zobod zirfid)))
```

We can print our data with the following:

```lisp
lfe> (p data)
;; (#(foo bar baz) #(quux quuz) #(corge grault garply) #(plugh xyzzy) #(flurb nirf) #(zobod zirfid))
;; ok
```

Or we can pretty-print it:

```lisp
lfe> (pp data)
;;(#(foo bar baz)
;; #(quux quuz)
;; #(corge grault garply)
;; #(plugh xyzzy)
;; #(flurb nirf)
;; #(zobod zirfid))
;; ok
```

The same may be done for displaying data in the Erlang format:

```lisp
lfe> (ep data)
;; [{foo,bar,baz},{quux,quuz},{corge,grault,garply},{plugh,xyzzy},{flurb,nirf},{zobod,zirfid}]
;; ok
```

```lisp
lfe> (epp data)
;; [{foo,bar,baz},
;;  {quux,quuz},
;;  {corge,grault,garply},
;;  {plugh,xyzzy},
;;  {flurb,nirf},
;;  {zobod,zirfid}]
;; ok
```
