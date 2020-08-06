# Modules

If some portion of your code is reusable enough to be a module then the maintenance gains are really worth the overhead of splitting it out with separate tests and docs.<a href="#footnote-1"><sup>1</sup></a>

Gains for separating code into separate modules include, but are not limited to:

* Easier reuse in other parts of the software being developed.
* Increased ability to reason about problems due to increased simplicity and separation of concerns.
* Great clarity and understanding of the system as a whole.

A good general workflow around module creation:

1. Start small and remain focused on the problem at hand.
1. Write just the functions you need.
1. Keep the functions small and limit them to one specific chunk of functionality (do one thing and do it well).
1. Make incremental changes as needed.

For new code:
1. Experiment with in the LFE REPL by defining your function and then calling with different values (expected and otherwise).
1. When it works in the REPL, create a test module in `./test` and paste the function calls in a test case.
1. Create a new module in `./src` and paste the final form of your function from the REPL.
4. Ensure the tests pass successfully for the new module.

[Build your libraries](libraries.html) using this approach

----

<ol>
<li><a name="footnote-1">
This entire page was adatped from a <a href="https://gist.github.com/substack/5075355">Github Gist</a> by <a href="https://github.com/substack">James Halliday</a>.
</li>
</ol>
