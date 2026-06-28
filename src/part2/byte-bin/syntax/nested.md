# Nested Binaries

You can nest binaries within binary construction:

```lfe
lfe> (let ((b1 #B(1 2))
        (b2 #B(3 4)))
    #B(b1 b2))
#B(1 2 3 4)
```

This concatenates the binaries, flattening them into a single contiguous sequence. It's the binary equivalent of appending lists, except more efficient because you're not walking linked structures and rebuilding chains of cons cells. You're just stacking memory blocks end-to-end like a particularly orderly game of digital Tetris.

The nested binary must, of course, be bound to a variable. Attempting to write `#B(#B(1 2))` will confuse the parser, much like attempting to say "this sentence is false" will confuse certain types of philosophers and most logical paradox detection systems.
