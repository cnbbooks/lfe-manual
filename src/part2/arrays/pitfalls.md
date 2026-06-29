# Common Pitfalls

**Off-by-one errors from zero-based indexing:**

```lfe
;; Wrong: trying to access the "fifth" element
lfe> (array:get 5 (array:from_list '(a b c d e)))
exception error: badarg

;; Right: the fifth element is at index 4
lfe> (array:get 4 (array:from_list '(a b c d e)))
e
```

**Forgetting that set returns a new array:**

```lfe
;; Wrong: the original array is unchanged
lfe> (let ((arr (array:new 10)))
       (array:set 5 "hello" arr)
       (array:get 5 arr))
undefined

;; Right: capture the returned array
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:set 5 "hello" arr1)))
       (array:get 5 arr2))
"hello"
```

**Attempting to set beyond a fixed array's bounds:**

```lfe
;; Wrong: fixed arrays don't grow
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:set 15 "too far" arr1)))
       arr2)
exception error: badarg

;; Right: use an extendible array or relax first
lfe> (let* ((arr1 (array:new 10))
            (arr2 (array:relax arr1))
            (arr3 (array:set 15 "just right" arr2)))
       (array:get 15 arr3))
"just right"
```
