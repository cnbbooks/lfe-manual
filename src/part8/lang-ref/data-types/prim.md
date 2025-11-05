# Primitive Types

| Type | Syntax | Example | Notes |
|------|--------|---------|-------|
| **Atom** | `'atom` or unquoted | `'hello`, `ok`, `foo-bar` | Interned symbols |
| **Integer** | Decimal, hex, binary | `42`, `#x2A`, `#b101010` | Arbitrary precision |
| **Float** | Decimal point or exponent | `3.14`, `1.5e10` | IEEE 754 double |
| **String** | Double quotes | `"hello"` | Actually a list of integers |
| **Binary** | `#"..."` | `#"bytes"` | Raw byte string |
| **Boolean** | `'true`, `'false` | `'true` | Just atoms |

**Atom Syntax**:

```lisp
;; Simple atoms (no quotes needed)
ok
error
foo
foo-bar
foo_bar

;; Quoted atoms (for special characters)
'hello world'
'|complex!@#$|'

;; Booleans are atoms
'true
'false
```

**Number Syntax**:

```lisp
;; Integers
42
-17
0

;; Different bases
#b1010        ; Binary: 10
#o755         ; Octal: 493
#x1A2F        ; Hex: 6703
#16rFF        ; Base 16: 255
#2r1010       ; Base 2: 10

;; Floats
3.14
-0.5
1.5e10
6.022e23
```

**String vs Binary**:

```lisp
;; String (list of integers)
"hello"
; → (104 101 108 108 111)

;; Binary (raw bytes)
#"hello"
; → <<"hello">>

;; Binary syntax
(binary ((str "hello")))
; → <<"hello">>
```
