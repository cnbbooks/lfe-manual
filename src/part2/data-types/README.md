# Primitive Types

This chapter covers the basic types of data available to LFE, upon which
primatives rest all LFE libraries and applications.

* Integers
* Floats
* Atoms
* Booleans
* Characters

Each of these types has an LFE test function of the form `TYPENAMEp` (which
wrap the respective Erlang `is_TYPENAME` function). These are used to perform
type checks (especially common in guard expressions). These predicate functions
will be covered in their respective type sections.
