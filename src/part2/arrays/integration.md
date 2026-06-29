# Integration with Other LFE Patterns

Arrays can be pattern-matched in function definitions, though the match is structural and depends on the internal representation:

```lfe
;; This works but is fragile and not recommended
(defun process-array
  (((tuple 'array _ size _ _))
   (list 'array-of-size size)))
```

Instead, use the array module functions:

```lfe
(defun process-array (arr)
  (if (array:is_array arr)
      (list 'array-of-size (array:size arr))
      'not-an-array))
```

Arrays integrate naturally with higher-order functions through their fold and map operations, allowing you to compose array operations with other functional patterns:

```lfe
(defun sum-of-squares (arr)
  (array:foldl
    (lambda (_ x acc)
      (+ acc (* x x)))
    0
    arr))
```
