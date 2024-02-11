# Review

We've got the whole rest of the book ahead of us to cover much of what you've seen in the sample project we've just created with our guessing game. In the coming pages, you will revisit every aspect of what you've seen so far in lots of detail with correspondingly useful instructions on these matters.

That being said, it would be unfair to not at least read through the code together and mention the high-level concepts involved. Since we only touched the code in one file, that will be the one that gets the most of our attention for this short review, but let's touch on the others here, too.

## Project Files

### `rebar.config`

This is the file you need in every LFE project you will write in order to take advantage of the features (and time-savings!) that `rebar3` provides. For this project, the two important parts are:

1. the entry for dependencies (only LFE in this case), and
1. the plugins entry for the LFE rebar3 plugin.

Project setup will be covered in Chapter XXX, section XXX.

## Source Files

The source files for our sample program in this walkthrough are for an OTP application. OTP-based projects will be covered in Chapter XXX, section XXX.

### `.app.src`

This file is mostly used for application metadata. Most of what our app uses in this file is pretty self-explanatory. Every LFE application will have one of these in the project source code. Every LFE library and application needs this file.

### `guessing-game-app.lfe`

This is the top-level file for our game, an OTP application. It only exports two functions: one to start the app and the other to stop it. The application is responsible for starting up whatever supervisors all your services/servers need. For this sample application, only one supervisor is needed (with a very simple supervision tree).

### `guessing-game-sup.lfe`

This module is a little more invloved and has all the configuration and code necessary to properly set up a supervisor for our server. When something goes wrong with our server, the restart strategy defined by our supervisor will kick in and get things back up and running again. This is one of the key secrets to OTP's wizardry, and we will be covering this in great detail later.

### `src/guessing-game.lfe`

This is the last file we'll look at, and is the one we'll cover in the most detail right now. Here's the entire content of what we created for our game:

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

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun initial-state () '#())
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         (initial-state)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (state)
  (random:seed (erlang:phash2 `(,(node)))
               (erlang:monotonic_time)
               (erlang:unique_integer))
  `#(ok ,state))

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

(defun handle_call
  (('stop _from state)
   `#(stop shutdown ok state))
  ((`#(echo ,msg) _from state)
   `#(reply ,msg state))
  ((message _from state)
   `#(reply ,(unknown-command) ,state)))

(defun handle_info
  ((`#(EXIT ,_from normal) state)
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((_msg state)
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; --------------
;;; our server API
;;; --------------

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

(defun start-game ()
  (gen_server:cast (SERVER) '#(start-game true)))

(defun stop-game ()
  (gen_server:cast (SERVER) '#(stop-game true)))

(defun guess (n)
  (gen_server:cast (SERVER) `#(guess ,n)))
```

The beginning of the file opens with a declaration of the module: not only its name, but the public functions we want to expose as part of our API. This will be covered in Chapter XXX, section XXX.

Next, we have a few constant functions. Functions are necessary here due to the fact that LFE does not have global variables. This will be covered in Chapter XXX, section XXX.

Then we define the functions that will be used as this module's implementation of a generic OTP server. There is some boilerplate here that will be discussed when we dive into LFE/OTP. This will be covered in Chapter XXX, section XXX.

After that, we define the functions that are used by the OTP machinery that will run our server. Here you see several examples of pattern matching function heads in LFE, a very powerful feature that lends itself nicely to consise and expressive code. This will be covered in Chapter XXX, section XXX.

Lastly, we define our own API. Most of these functions simply send messages to our running server. More on this in Chapter XXX, section XXX.
