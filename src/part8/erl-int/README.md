# Erlang Integration

LFE's defining characteristic is its **transparent, zero-overhead integration** with Erlang. This section documents how LFE achieves seamless interoperability while maintaining its Lisp identity.

**Core Principle**: LFE compiles to **identical BEAM bytecode** as Erlang.

**Key Implications**:

- No Foreign Function Interface (FFI) layer
- No marshaling or data conversion
- No performance penalty
- Complete access to Erlang ecosystem
- LFE and Erlang code are indistinguishable at runtime

**Design Decision**: Rather than implementing a Lisp *on top of* Erlang, LFE is a Lisp *that is* Erlang with different syntax.
