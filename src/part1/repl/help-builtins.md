# REPL Functions

Most of the LFE REPL functions are documented in stdlib reference for their [Erlang counterparts](http://erlang.org/doc/man/c.html). This section documents where the LFE REPL help diverges from the Erlang Shell help.

## Compilation

If you view the Erlang reference manual documentation for compiling files in the shell, you will see differences from what is show in the LFE help text. In particular, `(c)` is for compiling LFE modules and `(ec)` needs to be used for compiling Erlang source files.

In both cases, the resulting `.beam` files are compiled to the current working directory and not to an `ebin` directory. These `.beam` files will be found by LFE, since the current working directory is included in the path, but you'll likely want to perform some cleanup afterward.

## Documentation

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

Documentation for Erlang modules and fucntions is available via the [Command Interface](c.html)

## Printing Data

### LFE Formatting

LFE provides some nice convenience functions for displaying data structures in the REPL. Let's say we had a data structure defined thusly:

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

### Erlang Formatting

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
