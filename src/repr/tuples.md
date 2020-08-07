# Tuples and Proplists

Do not align keys and values in property lists; instead, simply use the standard Lisp formatting (e.g, as provided by the LFE Emacs formatter).

Bad:

```lisp
'(#(k1            v1)
  #(key2          value2)
  #(key-the-third value-the-third)
  #(another       one))
```

Better:

```lisp
'(#(k1 v1)
  #(key2 value2)
  #(key-the-third value-the-third)
  #(another one))
```
