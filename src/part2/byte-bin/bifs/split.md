# split-binary: The Precision Slicer

When you need to divide a binary into two parts at a specific position:

```lfe
(split-binary binary position)
```

This returns a tuple containing two binaries—the part before the position and the part from the position onward:

```lfe
lfe> (split-binary #B(1 2 3 4 5 6 7 8 9 10) 3)
#(#B(1 2 3) #B(4 5 6 7 8 9 10))
```

Note that the position is 0-indexed (unlike the list version), because different functions in the standard library have different opinions about whether counting should start at 0 or 1, and consistency is apparently for lesser languages. This is the kind of thing that makes perfect sense when you understand the historical context and which committee member was arguing for what during the standardization process, and makes no sense whatsoever if you expect software design to be guided by principles like "intuitive interface design" or "not making users remember special cases."

The first binary contains `position` bytes, and the second contains everything else. If you try to split at a position larger than the binary's size, you'll get an error of the "what did you expect to happen?" variety. If you split at position 0, you get an empty binary and the original binary. If you split at the size of the binary, you get the original binary and an empty binary. Edge cases are handled with all the enthusiasm of someone filing tax returns, which is to say, correctly but joylessly.
