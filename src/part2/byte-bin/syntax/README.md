# Basic Binary Syntax

In the beginning, there was `#B(...)`. Or possibly `<<...>>` if you're consulting Erlang documentation and wondering why everyone keeps using those cheerful angle brackets. Both notations refer to the same underlying concept, though LFE, having a certain aesthetic sensibility about parentheses, prefers the former. It's the difference between wearing a properly tailored suit and showing up in whatever was clean that morning—functionally equivalent, but one makes a better impression at job interviews.

The simplest possible binary is the empty one:

```lfe
lfe> #B()
#B()
```

This binary contains exactly nothing, which might seem like an odd thing to create deliberately, but philosophers have been creating nothing deliberately for millennia and they're considered rather clever. In practice, the empty binary occasionally proves useful as a starting point for accumulation patterns, or when you need to pass a binary but have no actual data. It's the binary equivalent of a polite nod of acknowledgment.
