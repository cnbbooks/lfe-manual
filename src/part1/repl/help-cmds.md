# REPL Commands

The LFE REPL provides several useful commands users:

```lisp
(reset-environment)             -- reset the environment to its initial state
(run file)                      -- execute all the shell commands in a <file>
(set pattern expr)
(set pattern (when guard) expr) -- evaluate <expr> and match the result with
                                   pattern binding
(slurp file)                    -- slurp in a LFE source <file> and makes
                                   everything available in the shell
(unslurp)                       -- revert back to the state before the last
                                   slurp
```

These are fairly self-explanatory, with the possible exception of clarifying how `run` and `slurp` differ:

* Calling `(run "some/file.lfe")` will cause the LFE REPL to read the contents of that file and then execute every line in that file as if they had been typed at the terminal. This is a convenient way of duplicating REPL state between sessions. (If you haven't kept track of your entries, you can always open up your [BEAM history file](readline.html) and create an `.lfe` file with all the required commands!)
* Calling `(slurp "some/other/file.lfe")` will place all functions, records, and macros defined in that file into the LFE environment, allowing you to call them without a `module:` prefix. Note that the code is not executed, but is instead placed into the current environment, ready for use.
