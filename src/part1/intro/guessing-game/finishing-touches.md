# Finishing Touches

There is one last thing we can do to make our game more interesting. Right now, the game will work. But every time we start up the REPL and kick off a new game, the same "random" number will be selected for the answer. In order to make things interesting, we need to generate a random seed when we initialize our server. 

We want to only do this once, though -- not every time the game starts, and certainly not every time a user guesses! When the LFE server supervisor starts our game server, one functions is called and called only once: `init/1`. That's where we want to make the change to support a better-than-default random seed.

Let's change that function:

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

Now we're ready to play!
