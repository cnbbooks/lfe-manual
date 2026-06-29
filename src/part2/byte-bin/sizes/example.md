# Practical Example: Fixed-Width Records

Imagine you're encoding a series of sensor readings where each reading consists of:

- 12-bit sensor ID
- 20-bit timestamp (milliseconds)

```lfe
(defun encode-reading (sensor-id timestamp)
  "Encode a sensor reading into a compact 32-bit (4-byte) binary."
  (binary ((sensor-id (size 12)) (timestamp (size 20)))))

(defun decode-readings (data)
  "Decode a stream of sensor readings."
  (binary-comp
    ((<< (id (size 12)) (ts (size 20)) >> (binary-gen (<= data))))
    (tuple id ts)))
```

Testing this delightful contraption:

```lfe
lfe> (set reading (encode-reading 2048 524288))
#B(128 8 0 0)

lfe> (decode-readings reading)
(#(2048 524288))
```
