# Pattern Matching Failures: When Things Go Splendidly Wrong

If you attempt to pattern match a tuple and the pattern doesn't fit—wrong size, wrong values, wrong phase of the moon—LFE will raise an exception:

```lfe
(let ((#(point x x) #(point 10 45)))  ; x can't be both 10 and 45
  'this-will-never-execute)
;; exception error: no match of right hand side value #(point 10 45)
```

This is actually a *feature*, not a bug. It's how you ensure that your functions receive the data they expect. If someone passes a rectangle to a function expecting a circle, you want to know about it immediately, not three hours later when the calculations have gone mysteriously wrong and you're debugging at 3 AM with nothing but cold coffee and regret for company.
