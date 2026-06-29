# Summary

These BIFs form the basic toolkit for binary manipulation in LFE. They're not particularly exciting—they convert things, split things, measure things, and serialize things. But combined with the bit syntax (which we'll explore next), they provide the foundation for remarkably sophisticated binary data manipulation.

The key insight is that the Erlang VM has spent decades optimizing these operations. When you call `term-to-binary`, you're not running some slow interpretation loop—you're calling hand-optimized C code that knows exactly how to serialize Erlang terms efficiently. When you split a binary, you're not copying data unnecessarily—the VM is using sophisticated memory management to share unchanged portions when possible.

This is the kind of optimization that only comes from real-world use at scale. The Erlang VM wasn't designed in an academic laboratory to be theoretically optimal—it was designed in telecommunications companies to handle millions of phone calls without falling over, which tends to focus one's attention rather effectively on what actually matters versus what sounds good in papers.

You get all of this for free, simply by using the provided BIFs. It's one of those situations where the lazy option (calling provided functions rather than implementing your own) is also the optimal option. This almost never happens in software engineering, which makes it worth celebrating when it does.
