# Servers

## Use generic functions for servers and protocol handlers wherever possible

In many circumstances it is a good idea to use generic server programs such as the generic server implemented in the standard libraries. Consistent use of a small set of generic servers will greatly simplify the total system structure.

The same is possible for most of the protocol handling software in the system.

## Write tail-recursive servers

All servers must be tail-recursive, otherwise the server will consume memory until the system runs out of it.

Don't program like this:

```lisp
(defun loop ()
  (receive
    (`#(msg1 ,msg1)
     ...
     (loop))
    ('stop 'true)
    (other
     (logger:error "Process ~w got unknown msg ~w~n"
                   `(,(self) ,other))
     (loop)))
  ;; don't do this! This is not tail-recursive!
  (io:format "Server going down" '()))
```

This is a correct solution:

```lisp
(defun loop ()
  (receive
    (`#(msg1 ,msg1)
     ...
     (loop))
    ('stop
     (io:format "Server going down" '()))
    (other
     (logger:error "Process ~w got unknown msg ~w~n"
                   `(,(self) ,other))
     (loop))))
```

If you use some kind of server library, for example generic, you automatically avoid doing this mistake.
