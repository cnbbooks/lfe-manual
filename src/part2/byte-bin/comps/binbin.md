# Binary to Binary: The Power Move

The most powerful use of binary comprehensions is transforming binaries into other binaries. Let's invert all the bytes:

```lfe
lfe> (binary-comp ((<<byte>> (binary-gen (<= #B(1 2 3 4 5)))))
                  (- 255 byte))
#B(254 253 252 251 250)
```

This inverts each byte by subtracting it from 255—a common operation in image processing (creating negatives) and certain encryption schemes (though please don't use this for actual encryption; that way lies security disasters).
