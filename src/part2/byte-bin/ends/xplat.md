# Cross-Platform Binary Protocols

When designing binary protocols for cross-platform use, **always explicitly specify `big`**. Network byte order is the universal standard, and failing to use it has caused countless bugs throughout computing history (though admittedly fewer than the confusion caused by time zones).

```lfe
(defun encode-packet (type-id data)
  "Encode a packet with proper network byte order."
  (let ((data-size (byte_size data)))
    (binary ((#xDEAD (size 16) big)              ; Magic number
             (type-id (size 8))                   ; Type (single byte, no endianness)
             (data-size (size 32) big)            ; Length field
             (data binary)))))                    ; Payload

(defun decode-packet (packet)
  "Decode a packet, expecting network byte order."
  (binary ((#xDEAD (size 16) big)
           (type-id (size 8))
           (data-size (size 32) big)
           (data (size data-size) binary)
           rest binary)
          packet)
  `#m(type ,type-id data ,data rest ,rest))
```
