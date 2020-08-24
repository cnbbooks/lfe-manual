# Command Interface

While many of the functions listed in the LFE `(help)` have their documentation in the [Erlang Command Interface module](http://erlang.org/doc/man/c.html) (CIM), not everything in the CIM has been provided in the LFE REPL, some of which can be useful at times.

Here are some of the more useful functions you may with to be aware of from that module:

* `(c:bt pid)` - Stack backtrace for a process. Equivalent to `(erlang:process_display pid 'backtrace)`.
* `(c:h mod)` - Print the documentation for `mod`
* `(c:h mod fun)` - Print the documentation for all `mod:fun`
* `(c:h mod fun arity)` - Print the documentation for `mod:fun/arity`
* `(c:lm)` - Reloads all currently loaded modules that have changed on disk (see `(c:mm)`). Returns the list of results from calling `(l mod)` for each such loaded module.
* `(c:memory)` - Memory allocation information. Equivalent to `(erlang:memory)`.
* `(c:mm)` -
* `(c:ni)` - Display system information, listing information about all nodes on the network
* `(c:nl mod)` - Loads Module on all nodes
* `(c:nregs)` - Displays information about all registered processes for all nodes in the network.
* `(c:uptime)` - Prints the node uptime (as specified by `(erlang:statistics 'wall_clock)`) in human-readable form.
* `(c:xm mod)` - Finds undefined functions, unused functions, and calls to deprecated functions in a module by calling xref:m/1.
