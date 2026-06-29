# Transformation Operations

## proplists:append_values/2

A curious function that wraps each value in a list unless it's already a list, then concatenates everything:

```lfe
lfe> (proplists:append_values 'include 
      '((include (one two)) (other 42) (include three) (include (four))))
(one two three four)
```

This is often useful for "incremental" options where you want to accumulate values across multiple sources. It's the functional programming equivalent of pushing items onto a stack, except the stack is inside-out and upside-down.

## proplists:compact/1

Minimizes the representation by converting everything to normal form:

```lfe
lfe> (proplists:compact '((a true) (b false) (c 42) d (e true)))
(a (b false) (c 42) d e)
```

This is equivalent to mapping `property/1` over the list, which means `{Key, true}` becomes just `Key` while everything else stays as it is. The result is aesthetically pleasing in the way that a freshly organized desk is aesthetically pleasing—you're not entirely sure what changed, but it looks better.

## proplists:unfold/1

The inverse of `compact/1`, expanding all atoms to explicit tuples:

```lfe
lfe> (proplists:unfold '(read write (mode binary)))
((read true) (write true) (mode binary))
```

This makes the implicit explicit, which is sometimes necessary when interfacing with systems that lack the sophistication to understand shorthand.
