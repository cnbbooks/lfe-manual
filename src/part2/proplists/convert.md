# Conversion Operations

## proplists:from_map/1

Converts a map to a proplist, because sometimes you need to interface with legacy code that hasn't discovered the joys of first-class key-value structures:

```lfe
lfe> (proplists:from_map (map 'timeout 5000 'retry 3 'verbose 'true))
((timeout 5000) (retry 3) (verbose true))
```

The resulting proplist preserves the map's key-value associations but loses the map's ordering guarantees. If a value in the map is the atom `true` and the key is also an atom, you might expect compaction to `{Key, true}` → `Key`, but that's not guaranteed—use `compact/1` if you want that.

## proplists:to_map/1,2

Converts a proplist to a map, normalizing optionally along the way:

```lfe
lfe> (proplists:to_map '(a (b 1) (c 2) (c 3)))
#M(a true b 1 c 2)

lfe> (proplists:to_map 
      '((no_verbose true) (colour red) (timeout 5000))
      '((negations ((no_verbose verbose)))
        (aliases ((colour color)))))
#M(verbose false color red timeout 5000)
```

Shorthand atoms are expanded to `Atom => true`. If the same key appears multiple times, the first occurrence wins—the value nearest to the head of the list, exactly as `get_value/2` would return. The two-arity version applies normalization stages before conversion, allowing you to clean up user input and convert to maps in one operation.
