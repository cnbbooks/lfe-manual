# Efficiency

When you construct a binary, the system allocates the appropriate amount of memory and copies the data into it. This is a relatively fast operation, but it's still worth being aware of. If you're building up a large binary piece by piece, you might want to accumulate the pieces in a list and construct the final binary all at once:

```lfe
(let ((pieces (list #B(1 2) #B(3 4) #B(5 6))))
  (list-to-binary pieces))
```

This is more efficient than repeatedly appending to a binary, because immutable data structures mean that each append operation creates a new binary. Better to collect everything and do one allocation than to do many allocations as you build up the structure incrementally.

Of course, premature optimization is the root of all evil (or at least the root of spending three weeks optimizing code that runs once at startup and accounts for 0.0001% of your execution time). But when dealing with large binaries in performance-critical code, this distinction can matter. The BEAM VM is fast, but physics still applies, and copying gigabytes of data byte-by-byte is still gigabytes of data that need to be copied.
