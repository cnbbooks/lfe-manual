# Ordering and Comparison

Maps have well-defined ordering semantics, which means you can sort them without inducing existential crises:

1. Maps with fewer entries are smaller than maps with more entries
2. Maps with equal sizes are compared by converting to sorted key-value lists

```lfe
(< '#M(a 1) '#M(a 1 b 2))
true

(< '#M(age 23 person "jim") '#M(email "sue@place.com" name "sue"))
true  ; because 'age < 'email
```

When comparing maps to other terms, maps are considered more complex than lists or tuples but simpler than functions and PIDs, in the grand ontological hierarchy of Erlang terms.
