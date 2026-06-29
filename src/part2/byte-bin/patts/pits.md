# Common Pitfalls and How to Avoid Them

## Pitfall 1: Spacing Around `<<`

```lfe
; Wrong - tokenizer reads this as ≤
(set b=<<1,2,3>>)

; Correct - space before <<
(set b (binary (1 2 3)))
```

In LFE, this particular pitfall is less common due to the explicit `binary` form, but it's worth noting if you're translating Erlang examples.

## Pitfall 2: Non-byte-aligned Binary Segments

```lfe
; This will fail - 7 bits is not byte-aligned
(binary ((x (size 7) binary) rest binary) data)

; This works - 7 bits as an integer or bitstring
(binary ((x (size 7)) rest binary) data)
```

Remember: segments typed as `binary` must be byte-aligned (size divisible by 8).

## Pitfall 3: Using Arithmetic in Patterns

```lfe
; This won't work - can't use arithmetic directly
(binary ((value (size (+ n 1)))) data)

; This works - compute size first
(let ((field-size (+ n 1)))
  (binary ((value (size field-size))) data))
```

Pattern matching expects literal values or bound variables, not arithmetic expressions. Do your math before the pattern, not during it.
