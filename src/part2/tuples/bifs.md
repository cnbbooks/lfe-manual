# Tuple BIFs: The Standard Library Weighs In

For those moments when pattern matching seems like too much work (a rare occurrence, to be sure), Erlang provides several Built-In Functions for tuple manipulation. In LFE, you call these using the module prefix:

```lfe
;; Get the size of a tuple
(tuple_size #(abc #(def 123) ghi))
;; Returns: 3

;; Extract an element by position (1-indexed, because apparently we're barbarians)
(element 2 #(abc #(def 123) ghi))
;; Returns: #(def 123)

;; Create a new tuple with a replaced element
(setelement 2 #(abc #(def 123) ghi) 'def)
;; Returns: #(abc def ghi)
```

Notice that `element` uses 1-based indexing, a decision that historians trace back to the original Erlang implementers' deep-seated distrust of the number zero. Or possibly it was just Tuesday and they were feeling contrary. History is unclear on this point.

Also note that `setelement` doesn't actually *set* anything—tuples are immutable, after all. Instead, it creates an entirely new tuple with your modification, leaving the original tuple to carry on with its life unbothered. This is functional programming: nothing ever changes, yet somehow everything is different.
