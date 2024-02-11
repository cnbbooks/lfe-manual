# Planning the Game

We've created our new project, but before we write even a single atom of code, let's take a moment to think about the problem and come up with a nice solution. By doing this, we increase our chances of making something both useful and elegant. As long as what we write remains legible and meets our needs, the less we write the better. This sort of practice elegance will make the code easier to maintain and reduce the chance for bugs (by the simple merrit of there being less code in which a bug may arise; the more code, the greater opportunities for bugs).

Our first step will be making sure we understand the problem and devising some minimal abstractions. Next, we'll think about what actually need to happen in the game. With that in hand, we will know what state we need to track. Then, we're off to the races: all the code will fall right into place and we'll get to play our game.

## Key Abstractions

In a guessing game, there are two players: one who has the answer, and one who seeks the answer. Our code and data should clearly model these two players.

## Actions

The player with the answer needs to peform the following actions:

1. At the beginning of the game, state the problem and tell the other player to start guessing
1. Receive the other player's guess
1. Check the guess against the answer
1. Report back to the other player on the provided guess
1. End the game if the guess was correct

The guessing player needs to take only one action:

1. guess!

## State

We need to track the state of the game. Based upon the actions we've examined, the overall state is very simple. Through the course of the game, will only need to preserve the answer that will be guessed.
