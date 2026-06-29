# Comparing Tuples: In Which Order Matters

Tuples can be compared using the standard comparison operators, and they obey a specific set of rules that would make a librarian weep with joy:

```lfe
(< #(1 2) #(1 3))     ;; => true (element 2 differs)
(< #(1 2) #(2 1))     ;; => true (element 1 differs)
(=:= #(a b) #(a b))   ;; => true (identical)
(=:= #(1 2) #(1 2 3)) ;; => false (different sizes)
```

Tuples are compared element by element, from left to right. The first differing element determines the result. If all elements are equal but the tuples are different lengths, the shorter tuple is considered "less than" the longer one, in accordance with the ancient wisdom that suggests brevity is somehow related to smallness.
