# Game API

In order to send a message to a running OTP server, you use special OTP functions for the type of server you are running. Our game is running a `gen_server` so we'll be using that OTP module to send messages, in particular we'll be calling `gen_server:cast`. However, creating messages and sending them via the appropriate `gen_server` function can get tedious quickly, so it is common practice to create API functions that do these things for you.

In our case, we want to go to the section with the heading `;;; our server API` and add the following:

```lisp
(defun start-game ()
  (gen_server:cast (SERVER) '#(start-game true)))

(defun stop-game ()
  (gen_server:cast (SERVER) '#(stop-game true)))

(defun guess (n)
  (gen_server:cast (SERVER) `#(guess ,n)))
```

Functions in LFE are private by default, so simply adding these functions doesn't make them publicly accessible. As things now stand these will not be usable outside their module; if we want to use them, e.g., from the REPL, we need to export them.

Go to the top of the `guessing-game` module and update the "server API" sectopm of the `export`s, chaning this:

```lisp
    ;; server API
    (pid 0)
    (echo 1)))
```

to this:

```lisp
    ;; server API
    (pid 0)
    (echo 1)
    (start-game 0)
    (stop-game 0)
    (guess 1)))
```

The final form of your module definition should look like this:

```lisp
(defmodule guessing-game
  (behaviour gen_server)
  (export
    ;; gen_server implementation
    (start_link 0)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
    ;; server API
    (pid 0)
    (echo 1)
    (start-game 0)
    (stop-game 0)
    (guess 1)))
```

Now our game functions are public, and we'll be able to use them from the REPL.
