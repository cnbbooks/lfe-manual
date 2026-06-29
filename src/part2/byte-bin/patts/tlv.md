# Complex Real-World Example: Decoding a TLV Structure

Type-Length-Value (TLV) encoding is ubiquitous in network protocols. Let's build a proper parser:

```lfe
(defun parse-tlv (data)
  "Parse TLV records: 1 byte type, 2 bytes length, N bytes value."
  (parse-tlv data '()))

(defun parse-tlv
  ; No more data
  ([('') acc]
   (lists:reverse acc))

  ; Parse one TLV record
  ([(binary ((type (size 8))
             (len (size 16) big)
             (value (size len) binary)
             rest binary))
    acc]
   (let ((record `#m(type ,type value ,value)))
     (parse-tlv rest (cons record acc))))

  ; Invalid data
  ([_ _acc]
   '#(error invalid_tlv)))
```

Testing with multiple TLV records:

```lfe
lfe> (set tlv-data
       (binary ((1 (size 8))           ; Type 1
                (5 (size 16) big)      ; Length 5
                (#"HELLO" binary)      ; Value "HELLO"
                (2 (size 8))           ; Type 2
                (5 (size 16) big)      ; Length 5
                (#"WORLD" binary))))   ; Value "WORLD"
#B(1 0 5 72 69 76 76 79 2 0 5 87 79 82 76 68)

lfe> (parse-tlv tlv-data)
(#m(type 1 value #B(72 69 76 76 79))
 #m(type 2 value #B(87 79 82 76 68)))
```
