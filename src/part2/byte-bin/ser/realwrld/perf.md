# Performance Considerations: The Need for Speed

Binary operations in Erlang/LFE are highly optimized, but there are still pitfalls:

## Good Practices

```lfe
;; DO: Build binaries in one go
(defun build-packet-good (header data checksum)
  (binary (header binary) (data binary) (checksum 32)))

;; DON'T: Build binaries piecemeal in a loop
(defun build-packet-bad (parts)
  (lists:foldl
    (lambda (part acc)
      (binary (acc binary) (part binary)))  ; Creates new binary each iteration
    (binary)
    parts))

;; DO: Use binary comprehensions for transformations
(defun double-bytes (bin)
  (binary ((<< (<< (* b 2) 8) >> || << (b 8) >> <= bin)>>>)))

;; DO: Match on the binary once, extract multiple fields
(defun parse-efficient (bin)
  (let (((binary (a 8) (b 16) (c 32) (rest binary)) bin))
    (tuple a b c rest)))

;; DON'T: Pattern match repeatedly on the same binary
(defun parse-inefficient (bin)
  (let (((binary (a 8) (_r1 binary)) bin)
        ((binary (_8) (b 16) (_r2 binary)) bin)
        ((binary (_8) (_16) (c 32) (rest binary)) bin))
    (tuple a b c rest)))
```

## Binary Memory Management

Erlang has two kinds of binaries:

1. **Heap binaries** (< 64 bytes): Stored on process heap, garbage collected normally
2. **Refc binaries** (≥ 64 bytes): Stored in separate area, reference counted

Sub-binaries created by pattern matching share data with the original binary, which is efficient but can prevent garbage collection of large binaries if you keep small references. For this reason:

```lfe
;; If you only need part of a large binary long-term, copy it:
(defun extract-header (large-binary)
  (let (((binary (header 256 binary) (_rest binary)) large-binary))
    ;; Copy header to new binary so large-binary can be GC'd
    (binary:copy header)))
```
