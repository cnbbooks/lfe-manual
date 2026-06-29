# The Two Operators: A Tale of Intention

Maps provide two operators for updates, each with its own philosophical stance on existence:

## The `=>` Operator: The Optimist

This operator believes in second chances and new beginnings. It will either:
* Update an existing key with a new value, or
* Add an entirely new key-value pair if the key doesn't exist

It always succeeds, much like an enthusiastic golden retriever.

```lfe
(set m3 (mupd m1 'c 'xx))
#M(a 1 b 2 c xx)
```

## The `:=` Operator: The Perfectionist

This operator has standards. It will:
* Update an existing key with a new value, but
* Raise an exception if you try to update a non-existent key

This is precisely the behavior you want when typos could ruin your afternoon.

```lfe
;; This works because 'a exists
(set m4 (mupd m1 'a 42))
#M(a 42 b 2)

;; This explodes with theatrical precision
(set m5 (mupd m1 'c 3))
;; ** exception error: bad argument
;;    key c does not exist in old map
```

The wisdom here is to use `=>` when establishing a key for the first time, and `:=` for all subsequent updates. This gives you both flexibility and type safety, which is roughly equivalent to having your cake and eating it too, except without the calories or the cake.
