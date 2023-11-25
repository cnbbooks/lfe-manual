# Introduction

## What is good style?

Good style in any language consists of code that is:[^1]

* Understandable
* Reusable
* Extensible
* Efficient
* Easy to develop and debug

It also helps ensure correctness, robustness, and compatibility. Maxims of good style are:

* Be explicit
* Be specific
* Be concise
* Be consistent
* Be helpful (anticipate the reader's needs)
* Be conventional (don't be obscure)
* Build abstractions at a usable level
* Allow tools to interact (referential transparency)

Know the context when reading code:

* Who wrote it and when?
* What were the business needs?
* What other factors contributed to the design decisions?

## Sources and Inspiration

The LFE Style Guide takes inspiration (and often times actual content) directly from key sources in the Lisp, Erlang, and even Clojure developer communities. These are as follows

* [Google Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml)
* [A Guide to Writing Good, Maintainable Common Lisp Code](https://lisp-lang.org/style-guide/)
* [Tutorial on Good Lisp Programming Style](http://norvig.com/luv-slides.ps)
* [Erlang Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)
* [Erlang Coding Standards & Guidelines](https://github.com/inaka/erlang_guidelines)
* [The Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide)
* [How to ns](https://stuartsierra.com/2016/clojure-how-to-ns.html)

Note, however, that these are not considered sacrosanct sources of ultimate truth; (and neither is this guide). Instead, they contain practices that we have either adopted as-is, modified to some extent, or simply rejected (e.g., due to prior conventions established in MACLISP and LMI Lisp, their inapplicability due to LFE's unique status as a Lisp _and_ Erlang dialect, etc.).

In general we suggest following the LFE style as outlined here if you are creating a new project. If you are contributing to a project maintained by someonoe in the community, we recommend consistency: using the style adopted by _that_ project (for any contributions to that project).

Above all, enjoy the parenthesis.

----

#### Notes

[^1] This section was adapted from the <a href="http://norvig.com/luv-slides.ps">Tutorial on Good Lisp Programming Style</a> by Peter Norvig and Kent Pitman.
