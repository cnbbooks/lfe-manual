# Accessing Values: The Proplists Module

The `proplists` module provides a comprehensive suite of functions for working with property lists. Each function approaches the fundamental problem of "find me this thing in this list" with slightly different assumptions about what you want and what should happen if the thing isn't there.

## proplists:get_value/2,3

The workhorse function, `get_value/2`, retrieves the value associated with a key, or returns `undefined` if the key doesn't exist:

```lfe
lfe> (proplists:get_value 'timeout '((timeout 5000) (retry 3)))
5000

lfe> (proplists:get_value 'nonexistent '((timeout 5000) (retry 3)))
undefined
```

The three-arity version allows you to specify a default value, which is useful when `undefined` is either too pessimistic or not sufficiently specific:

```lfe
lfe> (proplists:get_value 'timeout '((retry 3)) 1000)
1000

lfe> (proplists:get_value 'verbose '((silent true)) 'false)
false
```

For atoms serving as boolean flags, the value retrieved is simply `true`:

```lfe
lfe> (proplists:get_value 'binary '((active true) binary (packet 4)))
true
```

This is `lookup/2` doing its tuple-expansion magic behind the scenes, treating the atom as if it had been `{binary, true}` all along.

## proplists:get_bool/2

When you know you're dealing with boolean options and want a boolean answer without philosophical complications:

```lfe
lfe> (proplists:get_bool 'verbose '((verbose true) (timeout 5000)))
true

lfe> (proplists:get_bool 'verbose '((timeout 5000)))
false

lfe> (proplists:get_bool 'verbose '((verbose false)))
false
```

This function returns `true` only if `lookup/2` would yield `{Key, true}`. Everything else—absence, explicit `false`, the key existing with a different value—collapses to `false`. It's boolean logic for a universe that doesn't believe in maybe.

## proplists:get_all_values/2

For keys that appear multiple times (because sometimes repetition is emphasis, not error):

```lfe
lfe> (proplists:get_all_values 'header 
      '((method post) (header "Content-Type: text/plain") 
        (header "Authorization: Bearer token") (timeout 5000)))
("Content-Type: text/plain" "Authorization: Bearer token")

lfe> (proplists:get_all_values 'missing '((a 1) (b 2)))
()
```

This returns a list of all values associated with the specified key, preserving order. If no entries exist, you get the empty list, which is the Erlang way of saying "nothing to see here, move along."

## proplists:lookup/2

The most semantically honest function in the module, `lookup/2` returns exactly what it finds:

```lfe
lfe> (proplists:lookup 'timeout '((timeout 5000) (retry 3)))
(timeout 5000)

lfe> (proplists:lookup 'binary '((active true) binary))
(binary true)

lfe> (proplists:lookup 'nonexistent '((a 1) (b 2)))
none
```

For atoms in the list, it synthesizes the tuple `{Atom, true}`. If nothing is found, it returns the atom `none`, which is different from `undefined` in ways that matter only when you're debugging at 3 AM.

## proplists:lookup_all/2

Like `get_all_values/2`, but returns tuples instead of values:

```lfe
lfe> (proplists:lookup_all 'cookie 
      '((cookie "session=abc123") (timeout 1000) (cookie "user=arthur")))
((cookie "session=abc123") (cookie "user=arthur"))

lfe> (proplists:lookup_all 'missing '((a 1) (b 2)))
()
```

Useful when you need both the keys and the values, perhaps because you've forgotten which one you're working with and would like to see the whole thing laid out properly.
