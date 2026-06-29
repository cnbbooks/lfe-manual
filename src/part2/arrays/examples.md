# Working Examples

**Building an array incrementally:**

```lfe
(defun build-array (n)
  (build-array-helper 0 n (array:new)))

(defun build-array-helper (i n acc)
  (if (>= i n)
      acc
      (build-array-helper
        (+ i 1)
        n
        (array:set i (* i i) acc))))
```

```lfe
lfe> (array:to_list (build-array 10))
(0 1 4 9 16 25 36 49 64 81)
```

**Finding elements by predicate:**

```lfe
(defun array-find (pred arr)
  (array-find-helper pred arr 0 (array:size arr)))

(defun array-find-helper (pred arr i size)
  (cond
    ((>= i size) 'not-found)
    ((funcall pred (array:get i arr)) i)
    ('true (array-find-helper pred arr (+ i 1) size))))
```

```lfe
lfe> (let ((arr (array:from_list '(1 3 5 8 9 12))))
       (array-find (lambda (x) (== 0 (rem x 2))) arr))
3
```

**Filtering an array:**

```lfe
(defun array-filter (pred arr)
  (array:sparse_foldr
    (lambda (i x acc)
      (if (funcall pred x)
          (cons x acc)
          acc))
    '()
    arr))
```

```lfe
lfe> (let ((arr (array:from_list '(1 2 3 4 5 6 7 8 9 10))))
       (array-filter (lambda (x) (> x 5)) arr))
(6 7 8 9 10)
```
