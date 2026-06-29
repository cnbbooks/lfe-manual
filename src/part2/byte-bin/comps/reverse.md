# List to Binary: The Reverse Journey

We can just as easily generate binaries from lists:

```lfe
lfe> (binary-comp ((<<byte>> (list-gen (<- '(1 2 3 4 5))))) byte)
#B(1 2 3 4 5)
```

Note the subtle but crucial difference: we use `list-gen` with `<-` for list generators, and `binary-gen` with `<=` for binary generators. The mnemonic device here is that `<=` looks vaguely like it's dealing with binary data (or at least, it's different from `<-`, which is what matters).
