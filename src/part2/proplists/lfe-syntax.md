# Proplists in LFE: Syntactic Considerations

LFE's syntax makes proplists slightly more pleasant than in Erlang:

```lfe
;; Erlang
[{timeout, 5000}, {retry, 3}, binary]

;; LFE with quoting
'((timeout 5000) (retry 3) binary)

;; LFE with explicit list construction
(list '(timeout 5000) '(retry 3) 'binary)

;; LFE with backquote for computed values
`((timeout ,ms) (retry ,attempts) binary)
```

The backquote syntax is particularly useful when building options programmatically:

```lfe
(defun make-opts (timeout retry verbose?)
  `((timeout ,timeout)
    (retry ,retry)
    ,@(if verbose? '(verbose) '())))
```

The `,@` (unquote-splicing) operator allows conditional inclusion of options without nested conditionals or list concatenation.
