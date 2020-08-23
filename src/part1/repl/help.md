# REPL `(help)`

```lisp
lfe> (help)
```

```lisp
LFE shell built-in functions

(c file)       -- compile and load code in <file>
(cd dir)       -- change working directory to <dir>
(clear)        -- clear the REPL output
(doc mod)      -- documentation of a module
(doc mod mac)  -- documentation of a macro
(doc m f a)    -- documentation of a function
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

## Command Interface

Most of the functions listed in the LFE `(help)` info are aliases to the [Erlang Command Interface module](http://erlang.org/doc/man/c.html) (CIM) or are aliases to the things that the CIM points to. Not everything in the CIM has been provided in the LFE REPL, some of which can be useful at times.

* `(c:h 'mod)` - 
* `(c:h 'mod 'fun)` - 
* `(c:h 'mod 'fun arity)` - 
* `(c:lm)` - 
* `(c:memory)` - 
* `(c:mm)` - 
* `(c:uptime)` - 
* `(c:xm ...)` - 
