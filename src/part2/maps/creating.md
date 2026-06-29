# Creating Maps: The Art of Literal Expression

In LFE, as in life, there are multiple ways to express the same fundamental truth. For maps, the syntax bears a striking resemblance to records, save for the notable absence of a record name and the presence of the distinctly map-like operators `=>` and `:=`.

```lfe
;; A simple map, as maps go
(set m1 (map 'a 1 'b 2))
#M(a 1 b 2)

;; Or, if you prefer the Erlang-esque syntax
(set m2 '#M(a 1 b 2))
#M(a 1 b 2)

;; Maps with non-atomic keys, because why limit ourselves?
(set facts '#M((wife fred) "Sue"
               (age fred) 45
               (daughter fred) "Mary"
               (likes jim) ("cooking" "swimming" "reading")))
```

Note the delightful `#M` syntax, which is LFE's way of saying "behold, a map!" without resorting to semaphore flags. The `map` function provides an alternative construction method for those who prefer their parentheses in different arrangements.
