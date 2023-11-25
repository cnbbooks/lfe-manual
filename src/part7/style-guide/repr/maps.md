# Maps

Do not align keys and values in maps. Note, however, that the LFE Emacs formatter doesn't currently indent maps properly.

Bad:

```lisp
'#m(k1            v1
    key2          value2
    key-the-third value-the-third
    another       one)
```

Also bad (formatted with the LFE Emacs formatter):

```lisp
'#m(k1 v1
       key2          value2
       key-the-third value-the-third
       another       one)
```

Better:

```lisp
#m(k1 v1
   key2 value2
   key-the-third value-the-third
   another one)
```
