# Integrating into an Application

We're only going to touch one of the files that was generated when you created the `guessing-game` project: `./src/guessing-game.lfe`. You can ignore all the others. Once we've made all the changes summarized below, we will walk through this file at a high level, discussing the changes and how those contribute to the completion of the game.

First though, we need to reflect on the planning we just did, remembering the actions and states that we want to support. There's also another thing to consider, since we're writing this as is an always-up OTP app. With some adjustments for state magagement, it could easily be turned into something that literally millions of users could be accessing simultaneouslyi. So: how does a game that is usually implemented as a quick CLI toy get transformed in LFE/OTP such that it can be run as a server?

In short, we'll use OTP's `gen_server` capability ("behaviour") and the usual message-passing practices. As such, the server will need to be able to process the following messages:

* `#(start-game true)` (create a record to track game state)
* `#(stop-game true)` (clear the game state)
* `#(guess n)`
  * check for guess equal to the answer
  * greater than the answer, and
  * less than the answer

We could have just used atoms for the first two, and done away with the complexity of using tuples for those, but symmetry is nice :-)

To create the game, we're going to need to perform the following integration tasks:

* Update the `handle_cast` function to process the commands and guards we listed above
* Create API functions that cast the appropriate messages
* Update the `export` form in the module definition
* Set the random seed so that the answers are different every time you start the application
