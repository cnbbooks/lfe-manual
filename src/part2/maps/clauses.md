# Function Clauses: Maps as Dispatch Mechanism

Maps in function heads provide a delightfully expressive way to pattern match on specific key-value combinations:

```lfe
(defun count-characters (str)
  (count-characters str (map)))

(defun count-characters
  (((cons h t) (= (map h n) m))
   (count-characters t (mupd m h (+ n 1))))
  (((cons h t) m)
   (count-characters t (mupd m h 1)))
  (('() m)
   m))
```

This function counts character occurrences, building a map incrementally. The first clause fires when the character already exists (using `:=` implicitly), the second when it doesn't, and the third when we've run out of characters to count and might as well return what we've got.
