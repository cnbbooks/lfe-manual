# Complex Example: Run-Length Encoding

Let's implement a simple run-length encoder using binary comprehensions. This algorithm compresses sequences of repeated values:

```lfe
(defun rle-encode (data)
  "Simple run-length encoding for byte data."
  (rle-encode-helper data '()))

(defun rle-encode-helper
  ([(binary ()) acc]
   (pack-rle-pairs (lists:reverse acc)))

  ([(binary ((byte (size 8)) rest binary)) acc]
   (let ((count-and-rest (count-bytes byte rest 1)))
     (rle-encode-helper (element 2 count-and-rest)
                       (cons `#(,byte ,(element 1 count-and-rest)) acc)))))

(defun count-bytes (byte data count)
  "Count consecutive occurrences of byte."
  (case data
    ((binary (byte rest binary))
     (count-bytes byte rest (+ count 1)))
    (_
     `#(,count ,data))))

(defun pack-rle-pairs (pairs)
  "Pack RLE pairs into binary."
  (binary-comp ((<<(byte (size 8)) (count (size 8))>>
                 (list-gen (<- pairs))))
               (tuple byte count)))
```

Testing this contraption:

```lfe
lfe> (set input #B(65 65 65 65 66 66 67 68 68 68 68 68))
#B(65 65 65 65 66 66 67 68 68 68 68 68)  ; AAAABBCDDDDD

lfe> (rle-encode input)
#B(65 4 66 2 67 1 68 5)  ; A:4, B:2, C:1, D:5
```

Each pair of bytes in the output represents (value, count). This is more efficient when you have long runs of repeated values, though admittedly less efficient when you don't—rather like how a towel is useful when you're wet but somewhat burdensome when you're trying to pack light.
