# Practical Example: RGB Color Manipulation

Let's build a system for manipulating RGB pixels, because color manipulation is both useful and visually verifiable:

```lfe
(defun parse-pixels (binary-pixels)
  "Convert binary RGB data to a list of RGB tuples."
  (list-comp ((<<(r (size 8)) (g (size 8)) (b (size 8))>>
               (binary-gen (<= binary-pixels))))
             `#(,r ,g ,b)))

(defun pack-pixels (pixel-list)
  "Convert a list of RGB tuples to binary."
  (binary-comp ((<<(r (size 8)) (g (size 8)) (b (size 8))>>
                 (list-gen (<- pixel-list))))
               (tuple r g b)))

(defun grayscale (pixels)
  "Convert RGB pixels to grayscale using luminosity method."
  (binary-comp ((<<(r (size 8)) (g (size 8)) (b (size 8))>>
                 (binary-gen (<= pixels))))
               (let* ((luminosity (trunc (+ (* r 0.299)
                                           (* g 0.587)
                                           (* b 0.114))))
                      (gray (min 255 (max 0 luminosity))))
                 (tuple gray gray gray))))
```

Testing these functions:

```lfe
lfe> (set color-data #B(255 0 0  0 255 0  0 0 255))
#B(255 0 0 0 255 0 0 0 255)  ; Red, Green, Blue pixels

lfe> (parse-pixels color-data)
(#(255 0 0) #(0 255 0) #(0 0 255))

lfe> (grayscale color-data)
#B(76 76 76 149 149 149 29 29 29)  ; Grayscale equivalents
```

The grayscale conversion uses the standard luminosity formula, which weights green more heavily because the human eye is more sensitive to green light. This is one of those delightful facts of human biology that makes absolute sense once you know it, and seems arbitrary until you do.
