# Booleans

Strictly speaking, LFE has no Boolean type, just like Erlang. Instead, the atoms
`true` and `false` are treated as Booleans

``` lisp
lfe> (== 42 "the question")
false
lfe> (== 42 42)
true
lfe> (> 1 2)
false
lfe> (< 1 2)
true
```

## Operators

The standard logical operators are available to LFE Booleans:

``` lisp
lfe> (not 'true)
false
lfe> (not 'false)
true
lfe> (and 'true 'false)
false
lfe> (or 'true 'false)
true
lfe> (and 'true 'true 'true)
true
lfe> (and 'true 'true 'false)
false
lfe> (or 'false 'false 'false)
false
lfe> (or 'false 'false 'true)
true
lfe> (xor 'true 'true)
false
lfe> (xor 'true 'false)
true
lfe> (xor 'false 'false)
false
lfe> (xor 'false 'true)
true
```

With the `and` and `or` Boolean oprators, every argument is evaluated. To
accomodate situations where complex and possible expensive logical expressions
comprise the arguments to Boolean operators, short-circuit versions of these
functions are also provided:

* with `andalso`, returns as soon as the first `'false` is encountered;
* with `orelse`, returns as soon as the first `'true` is encountered.

To demonstrate this, we'll define a boolean function that prints to
`standard out` when it is evaluated:

``` lisp
(defun hey (x)
  (io:format "Made it here!~n") x)
```

Short-circuit demonstration of `andalso`:


``` lisp
lfe> (andalso 'true 'true 'false (hey 'true))
false
lfe> (andalso 'false 'true 'true (hey 'true))
false
lfe> (andalso 'true 'true 'true (hey 'true))
Made it here!
true
```

Short-circuit demonstration of `orelse`:

``` lisp
lfe> (orelse 'false 'false 'true (hey 'true))
true
lfe> (orelse 'true 'false 'false (hey 'true))
true
lfe> (orelse 'false 'false 'false (hey 'true))
Made it here!
true
```

## Predicates

To test if a value is a Boolean, we will first include some code:

``` lisp
lfe> (include-lib "lfe/include/cl.lfe")
```

That adds Common Lisp inspired functions and macros to our REPL session.

``` lisp
lfe> (booleanp 'true)
true
lfe> (booleanp 'false)
true
lfe> (booleanp 'arthur)
false
lfe> (booleanp 42)
false
```

If the atom in question has been used in a function name definition:

If you prefer the Clojure-style of predicates:

``` lisp
lfe> (include-lib "lfe/include/clj.lfe")
lfe> (boolean? 'true)
true
lfe> (boolean? 'false)
true
lfe> (boolean? 'arthur)
false
```

Of course there is always the Erlang predicate, usable without having to do any includes:

``` lisp
lfe> (is_boolean 'true)
true
lfe> (is_boolean 'false)
true
lfe> (is_boolean 'arthur)
false
```

Note that, since LFE Booleans are also atoms, these are valid as well:

``` lisp
lfe> (atomp 'true)
true
lfe> (atom? 'false)
true
lfe> (is_atom 'true)
true
```
