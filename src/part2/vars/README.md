# Variables

In LFE, variables are implemented with [atoms](../data-types/atoms.html). Atoms
are used in the language for such things as naming functions and macros, use as
keywords and in data structures. As you'll find out when reading about atoms,
they are evaluated as simply themselves. However, when they are used as variable
names, they evaulate to the value of which they were assigned.

There are two contexts for setting variables:

* in the [LFE REPL](../../part1/repl)
* in [LFE modules](../../part3/modules)

This distinction is important, not only because the contexts use different
forms, but because like Erlang, LFE does not support global variables.

This chpater will also introduce the reader to pattern-matching in LFE, as it
applies to variable assignment; a fuller discussion is presented in a
[dedicated chapter](../patterns) as well in the chapters that cover forms
which support pattern-matching (i.e., compound data types, functions, etc.).

Lastly we'll talk more about some of the LFE nuances around global variables.
