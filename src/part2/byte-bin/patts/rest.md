# The Rest of the Binary

Often you want to extract some fields and capture everything remaining in a catch-all variable. This is done by leaving off the size specification:

```lfe
(defun parse-header (packet)
  "Extract magic number and version, keep the rest."
  (binary ((magic (size 16)) (version (size 8)) payload binary) packet)
  (if (=:= magic #xCAFE)
      `#(ok ,version ,payload)
      '#(error invalid_magic)))
```

Testing:

```lfe
lfe> (set pkt (binary ((#xCAFE (size 16)) (42 (size 8)) (#"DATA" binary))))
#B(202 254 42 68 65 84 65)

lfe> (parse-header pkt)
#(ok 42 #B(68 65 84 65))
```

The `payload binary` at the end means "bind whatever's left to `payload`"—no size specification needed, as the size is implicitly "all remaining bytes."
