# Practical Use: Extraction and Division

```lfe
(defun unpack-rgb (color)
  "Unpack 24-bit color into RGB components."
  `#(,(band (bsr color 16) 255)
     ,(band (bsr color 8) 255)
     ,(band color 255)))

(defun fast-divide-by-power-of-two (n power)
  "Divide by 2^power using bit shift."
  (bsr n power))
```

Testing:

```lfe
lfe> (unpack-rgb 16744512)  ; #xFF8040
#(255 128 64)

lfe> (fast-divide-by-power-of-two 1000 3)
125  ; 1000 / 8 = 125
```
