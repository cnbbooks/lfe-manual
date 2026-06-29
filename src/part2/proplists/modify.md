# Modification Operations

## proplists:delete/2

Removes all entries associated with a key, because sometimes you need to thoroughly erase evidence:

```lfe
lfe> (proplists:delete 'timeout '((timeout 5000) (retry 3) (timeout 1000)))
((retry 3))

lfe> (proplists:delete 'verbose '((timeout 5000) (retry 3)))
((timeout 5000) (retry 3))
```

Note that this removes *all* occurrences. If you had multiple entries for the same key (perhaps for excellent reasons understood only by past-you), they all vanish with equal efficiency.
