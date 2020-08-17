# `handle_cast`

The biggest chunk of code that needs to be changed is the `handle_cast` function. Since our game doesn't return values, we'll be using `handle_cast`. (If we needed to have data or results returned to us in the REPL, we would have used `handle_call` instead. Note that both are standard OTP `gen_server` callback functions.)

The generated project barely populates this function and the function isn't of the form that supports patten-matching (which we need here) so we will essentially be replacing what was generated. In the file `./src/guessing-game.lfe`, change this:

```lisp
(defun handle_cast (_msg state)
  `#(noreply ,state))
```

to this:

```lisp
(defun handle_cast
  ((`#(start-game true) _state)
   (io:format "Guess the number I have chosen, between 1 and 10.~n")
   `#(noreply ,(random:uniform 10)))
  ((`#(stop-game true) _state)
   (io:format "Game over~n")
   '#(noreply undefined))
  ((`#(guess ,n) answer) (when (== n answer))
   (io:format "Well-guessed!!~n")
   (stop-game)
   '#(noreply undefined))
  ((`#(guess ,n) answer) (when (> n answer))
   (io:format "Your guess is too high.~n")
   `#(noreply ,answer))
  ((`#(guess ,n) answer) (when (< n answer))
   (io:format "Your guess is too low.~n")
   `#(noreply ,answer))
  ((_msg state)
   `#(noreply ,state)))
```

That is a single function in LFE, since for every match the arity of the function remains the same. It is, however, a function with six different and separate arguement-body forms: one for each pattern and/or guard.

These patterns are matched:

1. start
1. stop
1. guess (three times)
1. any

For the three guess patterns (well, one pattern, really) since there are three different guards we want placed on them:

1. guess is equal
1. guess is greater
1. guess is less

Note that the pattern for the function argument in these last three didn't change, only the guard is different beptween them.

Finally, there's the original "pass-through" or "match-any" pattern (this is used to prevent an error in the event of an unexpected message type).
