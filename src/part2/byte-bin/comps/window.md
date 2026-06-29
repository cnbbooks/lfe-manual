# Advanced Pattern: Sliding Window

Binary comprehensions can implement sliding window operations, useful for convolution filters or checksum calculations:

```lfe
(defun sliding-sum (data)
  "Calculate sum of each 3-byte window."
  (sliding-sum-helper data '()))

(defun sliding-sum-helper
  ([(binary ((a (size 8)) (b (size 8)) (c (size 8)) rest binary)) acc]
   (sliding-sum-helper
     (binary (b c rest binary))
     (cons (+ a b c) acc)))
  ([_ acc]
   (lists:reverse acc)))
```

Testing:

```lfe
lfe> (sliding-sum #B(1 2 3 4 5))
(6 9 12)  ; 1+2+3, 2+3+4, 3+4+5
```

This slides a 3-byte window across the data, computing the sum at each position. Real-world applications might compute moving averages, detect patterns, or implement digital signal processing filters.
