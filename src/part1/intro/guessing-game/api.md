# Game API

In order to send a message to a running OTP server, you use special OTP functions for the type of server you are running. Our game is running a `gen_server` so we'll be using use that OTP module to send messages.

Creating messages and sending them via the appropriate `gen_server` function can get tedious quickly, so it is common practice to create API functions that do these things for you.

In our case, we want to go to the section with the heading `;;; our server API` and add the following:

```lisp
(defun start-game ()
  (gen_server:cast (SERVER) '#(start-game true)))

(defun stop-game ()
  (gen_server:cast (SERVER) '#(stop-game true)))

(defun guess (n)
  (gen_server:cast (SERVER) `#(guess ,n)))
```

We have added these functions, but they will not be usable outside the module, since functions in LFE are private by default. In order for other modules (or a user in the REPL) to use our functions, we need to export them.

Go to the top of the `guessing-game` module and change this:

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
    (echo 1)))
```

to this:

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
