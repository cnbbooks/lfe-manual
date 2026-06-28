# Binary BIFs

The Erlang runtime system, in its infinite wisdom (or at least its well-considered pragmatism developed over three decades of telecommunications infrastructure), provides a collection of Built-In Functions—BIFs, to those in the know—for manipulating binaries. These are not functions written in Erlang or LFE themselves, but rather direct calls into the runtime's implementation in C, which means they're fast in the way that things are fast when they don't have to worry about abstractions or politeness and can just get on with the business of moving bytes around memory.

Think of BIFs as the difference between asking someone nicely to complete a task and simply reaching into their brain and adjusting the relevant neurons directly. More efficient, certainly, though probably less socially acceptable in most cultures.
