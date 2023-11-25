# Libraries

## Use Before You Write

Look for libraries that solve the problems you are trying to solve before embarking on a project. <a href="#footnote-1"><sup>1</sup></a> Making a project with no dependencies is not some sort of virtue. It doesn’t aid portability and it doesn’t help when it comes to turning a Lisp program into an executable.

Writing a library that solves the same problem as another hurts consolidation. Generally, you should only do this if you are going to make it worth it: Good, complete documentation, examples, and a well-designed website are – taken together – a good reason to write an alternative library to an existing one.

As always, check the licensing information of the libraries you use for incompatibilities.

## Writing Libraries

Before starting a project, think about its structure: Does every component have to be implemented within the project? Or are there parts that might be usable by others? If so, split the project into libraries.

If you set out to write a vast monolith and then lose interest, you will end up with 30% of an ambitious project, completely unusable to others because it’s bound to the rest of the unfinished code.

If you think of your project as a collection of independent libraries, bound together by a thin layer of domain-specific functionality, then if you lose interest in a project you will have left a short trail of useful libraries for others to use and build upon.

In short: write many small libraries.

----

<ol>
<li><a name="footnote-1">
This entire page was adapted from the lisp-lang.org Style Guide's <a href="https://lisp-lang.org/style-guide/#general-guidelines">General Guidelines</a>.
</li>
</ol>
