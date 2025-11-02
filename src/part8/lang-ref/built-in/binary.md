# Binary Operations

| Function | Syntax | Description |
|----------|--------|-------------|
| `binary` | `(binary seg ...)` | Create binary |
| `byte_size` | `(byte_size b)` | Binary size in bytes |
| `bit_size` | `(bit_size b)` | Binary size in bits |
| `binary_to_list` | `(binary_to_list b)` | Convert to list |
| `list_to_binary` | `(list_to_binary l)` | Convert from list |

**Binary Syntax**:

```lisp
;; Simple binary
(binary (42))                    ; → <<42>>

;; Sized segments
(binary (42 (size 16)))          ; → <<0,42>> (16-bit)

;; Multiple segments
(binary (1) (2) (3))             ; → <<1,2,3>>

;; Type specifiers
(binary (3.14 (float 32)))       ; → 32-bit float

;; String
(binary ((str "hello")))         ; → <<"hello">>

;; Concatenation
(let ((b1 (binary (1) (2)))
      (b2 (binary (3) (4))))
  (binary (b1 (binary))
          (b2 (binary))))        ; → <<1,2,3,4>>
```
