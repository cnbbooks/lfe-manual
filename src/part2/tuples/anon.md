# The Anonymous Variable: Underscore of Mystery

Sometimes you don't care about certain parts of a tuple. Perhaps you only need the X coordinate from a point and the Y can go hang. For such occasions, LFE provides the anonymous variable, written as `_` (that's an underscore, not a very short horizontal line):

```lfe
(let ((#(point x _) point))
  x)
```

The underscore is special. Unlike regular variables, which get rather upset if you try to bind them to two different values simultaneously, the anonymous variable accepts whatever you throw at it with the serene indifference of a cat watching television. You can use `_` multiple times in the same pattern, and each occurrence can match a different value. It's the perfect placeholder for data you don't care about—the "etc." of the pattern matching world.
