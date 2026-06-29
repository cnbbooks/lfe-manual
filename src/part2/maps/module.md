# The Maps Module: A Compendium of Useful Functions

LFE provides access to all of Erlang's `maps` module functions. Here are the essential ones, annotated for your convenience:

## maps:new/0
Creates an empty map. Philosophically equivalent to a blank canvas or an unwritten novel.

```lfe
(maps:new)
#M()
```

## maps:size/1
Returns the number of key-value pairs, useful for determining if your map has achieved critical mass.

```lfe
(maps:size '#M(a 1 b 2 c 3))
3
```

## maps:get/2
Retrieves a value or raises an exception, rather like a particularly strict librarian.

```lfe
(maps:get 'a '#M(a 1 b 2))
1

(maps:get 'z '#M(a 1 b 2))
;; ** exception error: bad_key
```

## maps:find/2
The polite version of `get`, returning `(tuple 'ok value)` or the atom `error`.

```lfe
(maps:find 'a '#M(a 1 b 2))
#(ok 1)

(maps:find 'z '#M(a 1 b 2))
error
```

## maps:is_key/2
A simple existential check that returns a boolean without drama.

```lfe
(maps:is_key 'a '#M(a 1 b 2))
true

(maps:is_key 'z '#M(a 1 b 2))
false
```

## maps:keys/1
Returns all keys in ascending order, neatly arranged like soldiers on parade.

```lfe
(maps:keys '#M(c 3 a 1 b 2))
(a b c)
```

## maps:values/1
Returns all values in key-sorted order, which is to say, the values corresponding to the sorted keys.

```lfe
(maps:values '#M(c 3 a 1 b 2))
(1 2 3)
```

## maps:to_list/1
Converts a map to a list of two-tuples, preserving key order.

```lfe
(maps:to_list '#M(a 1 b 2))
(#(a 1) #(b 2))
```

## maps:from_list/1
The inverse operation, constructing a map from tuple pairs. If duplicate keys exist, the first value wins—first come, first served, as it were.

```lfe
(maps:from_list '(#(a 1) #(b 2) #(a 99)))
#M(a 1 b 2)
```

## maps:remove/2
Removes a key-value pair, if it exists. Idempotent and unbothered.

```lfe
(maps:remove 'b '#M(a 1 b 2 c 3))
#M(a 1 c 3)
```

## maps:merge/2
Combines two maps, with the second map's values taking precedence in key conflicts. Excellent for establishing defaults:

```lfe
(let ((defaults '#M(timeout 5000 retry 3 verbose false))
      (user-opts '#M(timeout 1000 verbose true)))
  (maps:merge defaults user-opts))
;; #M(retry 3 timeout 1000 verbose true)
```

## maps:map/2
Applies a function to all values while preserving keys, rather like redecorating without changing the floor plan.

```lfe
(maps:map (lambda (k v) (* v 2)) '#M(a 1 b 2 c 3))
#M(a 2 b 4 c 6)
```

## maps:filter/2
Keeps only the key-value pairs satisfying a predicate, in the finest tradition of selective attention.

```lfe
(maps:filter (lambda (k v) (> v 2)) '#M(a 1 b 2 c 3 d 4))
#M(c 3 d 4)
```
