# Higher-Order Functions

Arrays support the traditional triumvirate of functional programming operations: map, fold, and their various cousins who show up uninvited to family gatherings.

**Mapping:**

`array:map` applies a function to each element, returning a new array:

```lfe
lfe> (let ((arr (array:from_list '(1 2 3 4 5))))
       (array:to_list
         (array:map
           (lambda (i x) (* x x))
           arr)))
(1 4 9 16 25)
```

Note that the mapping function receives both the index and the value, which is useful for operations that care about position. If you don't need the index, simply ignore it with underscore-based nonchalance:

```lfe
lfe> (let ((arr (array:from_list '("hello" "world" "goodbye"))))
       (array:to_list
         (array:map
           (lambda (_ s) (string:uppercase s))
           arr)))
("HELLO" "WORLD" "GOODBYE")
```

For sparse mapping (skipping default values), use `array:sparse_map`:

```lfe
lfe> (let* ((arr1 (array:new 100))
            (arr2 (array:set 10 5 arr1))
            (arr3 (array:set 50 10 arr2)))
       (array:sparse_to_orddict
         (array:sparse_map
           (lambda (i x) (* x 2))
           arr3)))
(#(10 10) #(50 20))
```

**Folding (left):**

`array:foldl` processes the array from index 0 upward, accumulating a result:

```lfe
lfe> (let ((arr (array:from_list '(1 2 3 4 5))))
       (array:foldl
         (lambda (i x acc) (+ x acc))
         0
         arr))
15
lfe> (let ((arr (array:from_list '("the" "answer" "is" "42"))))
       (array:foldl
         (lambda (i word acc)
           (++ acc " " word))
         ""
         arr))
" the answer is 42"
```

The fold function receives the index, the value, and the accumulator, in that order. This can be slightly disorienting if you're accustomed to lists, where the fold function typically takes just the value and accumulator, but arrays like to remind you that they know where they are at all times.

**Folding (right):**

`array:foldr` processes from the highest index downward:

```lfe
lfe> (let ((arr (array:from_list '(1 2 3 4 5))))
       (array:foldr
         (lambda (i x acc) (cons x acc))
         '()
         arr))
(1 2 3 4 5)
lfe> (let ((arr (array:from_list '(1 2 3 4 5))))
       (array:foldl
         (lambda (i x acc) (cons x acc))
         '()
         arr))
(5 4 3 2 1)
```

**Sparse folding:**

Both `array:sparse_foldl` and `array:sparse_foldr` skip default-valued entries:

```lfe
lfe> (let* ((arr1 (array:new 100))
            (arr2 (array:set 10 "alpha" arr1))
            (arr3 (array:set 50 "omega" arr2)))
       (array:sparse_foldl
         (lambda (i x acc) (cons x acc))
         '()
         arr3))
("omega" "alpha")
```
