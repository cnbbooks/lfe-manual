# `(help)`

As you gain familiarity with the LFE REPL, one of the most useful and convenient references will be the summary of functions, commands, and variablese that come with the LFE REPL.

To see these, simple call the `help` or `h` function:

```lisp
lfe> (help)
```

That will result in the following being displyed to your terminal:

```lisp
LFE shell built-in functions

(c file)       -- compile and load code in <file>
(cd dir)       -- change working directory to <dir>
(clear)        -- clear the REPL output
(doc mod)      -- documentation of a module
(doc mod:mac)  -- documentation of a macro
(doc m:f/a)    -- documentation of a function
(ec file)      -- compile and load code in erlang <file>
(ep expr)      -- print a term in erlang form
(epp expr)     -- pretty print a term in erlang form
(exit)         -- quit - an alias for (q)
(flush)        -- flush any messages sent to the shell
(h)            -- an alias for (help)
(help)         -- help info
(i)            -- information about the system
(i pids)       -- information about a list of pids
(l module)     -- load or reload <module>
(ls)           -- list files in the current directory
(ls dir)       -- list files in directory <dir>
(m)            -- which modules are loaded
(m mod)        -- information about module <mod>
(p expr)       -- print a term
(pp expr)      -- pretty print a term
(pid x y z)    -- convert <x>, <y> and <z> to a pid
(pwd)          -- print working directory
(q)            -- quit - shorthand for init:stop/0
(regs)         -- information about registered processes

LFE shell built-in commands

(reset-environment)             -- reset the environment to its initial state
(run file)                      -- execute all the shell commands in a <file>
(set pattern expr)
(set pattern (when guard) expr) -- evaluate <expr> and match the result with
                                   pattern binding
(slurp file)                    -- slurp in a LFE source <file> and makes
                                   everything available in the shell
(unslurp)                       -- revert back to the state before the last
                                   slurp

LFE shell built-in variables

+/++/+++      -- the tree previous expressions
*/**/***      -- the values of the previous expressions
-             -- the current expression output
$ENV          -- the current LFE environment

ok
```

Most of those are documented in stdlib reference for their [Erlang counterparts](http://erlang.org/doc/man/c.html), so be sure to reference that information for details on many of the above.

Those not covered in that Erlang reference manual, or those that are different in their LFE versionsm, include:

* Built-in Functions
  * Compilation functions
  * LFE code documentation
  * Printing and pretty-printing
* Built-in commands
* Built-in variables
