# Pattern Matching with Endianness

Endianness specifiers work identically in both construction and pattern matching:

```lfe
(defun decode-network-header (packet)
  "Decode a network packet header with big-endian values."
  (binary ((magic (size 16) big)
           (length (size 32) big)
           (flags (size 16) big)
           payload binary)
          packet)
  `#m(magic ,magic length ,length flags ,flags payload ,payload))

(defun decode-local-data (data)
  "Decode platform-specific data with native endianness."
  (binary ((timestamp (size 64) native)
           (value (size 32) native)
           rest binary)
          data)
  `#m(timestamp ,timestamp value ,value rest ,rest))
```

Testing these functions:

```lfe
lfe> (set packet (binary ((#xCAFE (size 16) big)
                          (1024 (size 32) big)
                          (#xFF00 (size 16) big)
                          (#"DATA" binary))))
#B(202 254 0 0 4 0 255 0 68 65 84 65)

lfe> (decode-network-header packet)
#m(magic 51966 length 1024 flags 65280 payload #B(68 65 84 65))
```
