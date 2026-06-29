# Partitioning and Splitting

## proplists:split/2

Partitions a proplist into sublists based on specified keys:

```lfe
lfe> (proplists:split 
      '((c 2) (e 1) a (c 3 4) d (b 5) b)
      '(a b c))
(((a) ((b 5) b) ((c 2) (c 3 4))) ((e 1) d))
```

The result is a tuple `{Lists, Rest}` where `Lists` contains one sublist for each key (in the order specified), and `Rest` contains everything else. Relative order is preserved within each sublist, which means this operation is stable and predictable—rare qualities in a chaotic universe.
