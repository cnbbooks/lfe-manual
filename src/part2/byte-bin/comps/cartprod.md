# Multiple Generators: The Cartesian Product

Binary comprehensions can have multiple generators, producing the Cartesian product of their inputs:

```lfe
lfe> (binary-comp ((<<x>> (binary-gen (<= #B(1 2))))
                   (<<y>> (binary-gen (<= #B(10 20)))))
                  (+ x y))
#B(11 21 12 22)
```

This generates all combinations: (1+10), (1+20), (2+10), (2+20). While this example is contrived (as examples often are, being designed for clarity rather than immediate utility), the principle is powerful when dealing with structured data like coordinate grids or lookup tables.
