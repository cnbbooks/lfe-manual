# Extracting Values: Pattern Matching to the Rescue

Now we arrive at one of Erlang's (and by extension, LFE's) most celebrated features, the one that makes functional programmers go all misty-eyed and C programmers reach nervously for their pointer arithmetic: **pattern matching**.

Suppose you have a tuple and you want to know what's in it:

```lfe
(set point #(point 10 45))
```

In lesser languages, you might extract the X coordinate with something tedious like `get-tuple-element-2` or `point.x` (oh, the mundanity!). But we are not using lesser languages. We are using LFE, where pattern matching reigns supreme:

```lfe
(let ((#(point x y) point))
  (list x y))
```

This remarkable bit of syntactic sorcery does several things simultaneously:

1. It checks that `point` is indeed a three-element tuple
2. It verifies that the first element is the atom `point`
3. It binds the second element to the variable `x`
4. It binds the third element to the variable `y`
5. It evaluates the body of the `let`, where we can now use `x` and `y` with reckless abandon

If the pattern doesn't match—if, say, our tuple was `#(rectangle 10 45)` instead—the system will throw an exception with all the diplomatic grace of a bouncer ejecting a philosophy student from a nightclub.
