# Substitution and Expansion

## proplists:substitute_aliases/2

Allows you to rename keys systematically, useful when your API needs to support both British and American spellings:

```lfe
lfe> (proplists:substitute_aliases 
      '((color colour) (center centre))
      '((color red) (size large) (center middle)))
((colour red) (size large) (centre middle))
```

The aliases list consists of `{OldKey, NewKey}` pairs. If the same old key appears multiple times, only the first alias is used—first come, first served, as civilized society demands.

## proplists:substitute_negations/2

A delightfully specific function that both renames keys and negates boolean values:

```lfe
lfe> (proplists:substitute_negations
      '((no_color color) (disable_cache cache))
      '((no_color true) (timeout 5000) (disable_cache false)))
((color false) (timeout 5000) cache)
```

This handles the common pattern where options can be expressed in negative form (`no_foo`, `disable_bar`) and need to be normalized to positive form with inverted values. Note that `{disable_cache, false}` becomes just `cache`, because the double negative resolves to an affirmative shorthand.

## proplists:expand/2

Expands property macros into their full forms, allowing you to define shorthand notations:

```lfe
lfe> (proplists:expand
      '((debug ((verbose true) (log_level debug) (trace true))))
      '((timeout 5000) debug (retry 3)))
((timeout 5000) (verbose true) (log_level debug) (trace true) (retry 3))
```

The expansion list contains `{Property, Expansion}` pairs. When a property matches, it's replaced by the expansion terms. If you want the original property preserved, include it in the expansion. Expansions are not recursive—once expanded, the terms are considered done.

## proplists:normalize/2

Passes a proplist through a pipeline of transformations, allowing you to build complex normalization strategies:

```lfe
lfe> (proplists:normalize
      '((timeout 5000) no_verbose (colour red))
      '((negations ((no_verbose verbose)))
        (aliases ((colour color)))
        (expand ((debug ((verbose true) (log_level debug)))))))
((timeout 5000) (verbose false) (color red))
```

The stages are processed in order, and the result is automatically compacted. Typical usage involves:

1. **Negations first**: Convert `no_foo` to `{foo, false}`
2. **Aliases second**: Normalize variant spellings
3. **Expansions last**: Expand shorthand to full options

This is the functional equivalent of data cleaning, where messy user input becomes pristine internal representation.
