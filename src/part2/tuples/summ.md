# Summary: Tuples in Brief

Tuples are:
- Fixed-size collections of elements
- Written with `#(...)` in LFE
- Often tagged with an atom in the first position
- Immutable (like everything else in LFE)
- Excellent for pattern matching
- Compared element-by-element
- Stored efficiently in contiguous memory
- The go-to choice for fixed-size data structures

They are not:
- Variable-size (that's what lists are for)
- Mutable (did we mention immutability yet?)
- Named (that would be records, which are basically tuples in a fancy hat)
- Particularly good at explaining themselves without tags

And there you have it: tuples in all their curly-braced glory. Use them wisely, use them well, and for goodness' sake, remember to tag them properly. Your future self will thank you, probably while muttering about how past-you was surprisingly competent for once.

Now then, shall we move on to lists? They're like tuples, except completely different and infinitely more recursive.
