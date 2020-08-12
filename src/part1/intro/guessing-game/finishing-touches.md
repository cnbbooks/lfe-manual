# Finishing Touches

There is one last thing we can do to make our game more interesting. Right now, the game will work. But every time we start up the REPL and kick off a new game, the same "random" number will be selected for the answer. In order to make things interesting, we need to generate a random seed when we initialize our server. You can do that by changing this:

```lisp
(defun init (state)
  `#(ok ,state))
```

to this:

```lisp
(defun init (state)
  (random:seed (erlang:phash2 `(,(node)))
               (erlang:monotonic_time)
               (erlang:unique_integer))
  `#(ok ,state))
```
