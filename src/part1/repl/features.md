# Core Features

In keeping with the overall herritage of LFE, its REPL is both a Lisp REPL as well as an Erlang shell. In fact, when support was added for the LFE REPL to the `rebar3_lfe` plugin, it utilised _all_ of the plumbing for the Erlang shell support in `rebar3`.

For the developer, though, this means that the LFE REPL holds a dual set of features, multiplying the set of features available to just the Erlang shell. These features include the following support:

* Evaluation of Lisp S-expressions
* Definition of functions, completely with LFE tail-recursion support
* Definition of records and use of record-specific support functions/macros
* Creation of LFE macros via standard Lisp-2 syntax
* Macro examination and debugging with various expansion macros
* The ability to start the LFE REPL in distribution mode, complete with Erlang cookie, and thus to not only access remote LFE and Erlang nodes, but to be accessed as a remote node itself (for nodes that have been granted access
* Access to the Erlang JCL and the ability to start separate, LFE shells running concurrently

## Unsupported

The following capabilities are not supported in the LFE REPL:

* Module definitions; these are a file-based feature in LFE, just as with Erlang.
