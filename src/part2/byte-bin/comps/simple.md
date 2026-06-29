# Binary to List: The Simplest Case

Let's extract individual bytes from a binary and convert them to a list:

```lfe
lfe> (list-comp ((<byte> (binary-gen (<= #B(1 2 3 4 5))))) byte)
(1 2 3 4 5)
```

The pattern `(<byte>` extracts each byte in sequence. We could be more explicit about the size:

```lfe
lfe> (list-comp ((<<(byte (size 8))>> (binary-gen (<= #B(72 101 108 108 111)))))
                byte)
(72 101 108 108 111)  ; ASCII codes for "Hello"
```

## Adding Filters

Binary comprehensions support filters just like list comprehensions. Let's extract only the even bytes:

```lfe
lfe> (list-comp ((<<byte>> (binary-gen (<= #B(1 2 3 4 5 6 7 8))))
                 (=:= 0 (rem byte 2)))
                byte)
(2 4 6 8)
```

The guard `(=:= 0 (rem byte 2))` filters for even numbers. This is considerably more elegant than manually iterating through the binary with index arithmetic and conditional logic, rather like how using a proper can opener is more elegant than attacking a tin with a rock.
