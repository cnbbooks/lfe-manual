# Practical Examples

## Reading a BMP File Header

Bitmap files use little-endian encoding (because they were designed by Microsoft, which runs on Intel):

```lfe
(defun read-bmp-header (file-data)
  "Read BMP file header (little-endian format)."
  (binary ((#"BM" binary)                        ; Magic bytes
           (file-size (size 32) little)          ; File size
           (_ (size 16) little)                  ; Reserved
           (_ (size 16) little)                  ; Reserved
           (pixel-offset (size 32) little)       ; Offset to pixel data
           rest binary)
          file-data)
  `#m(file-size ,file-size pixel-offset ,pixel-offset data ,rest))
```

## Reading a PNG File Header

PNG files use big-endian encoding (network byte order):

```lfe
(defun read-png-header (file-data)
  "Read PNG file header (big-endian format)."
  (binary ((#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A)  ; PNG signature
           (chunk-length (size 32) big)                ; Length
           (chunk-type (size 32) binary)               ; Chunk type
           rest binary)
          file-data)
  `#m(length ,chunk-length type ,chunk-type data ,rest))
```
