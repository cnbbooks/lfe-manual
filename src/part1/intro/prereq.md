# Prerequisites

Anyone coming to LFE should have experience programming in another language, ideally a systems programming language, especially if that language was found lacking. If the corageous reader is attmping to use LFE as a means of entering the study of computer science, we might offer several _other_ paths of study which may bear fruit more quickly and with less pain.

No prior Lisp experience is required, but that would certinaly be helpful. The same goes for Erlang/OTP (or any of the suite of BEAM languages). The reader with experience writing concurrent applications, wrestling with fault-tolerance, or maintaining highly-available applications and services does receive bonus points for preparedness. Such well-prepared readers landing here may have, in fact, done so due to a quest for a distributed Lisp. For those whom this does apply, your quest has found its happy end.

This book assumes the reader has the following installed upon their system:

* a package manager for easily installing software (in particular, development tools and supporting libraries)
* `git`, `make`, and other core open source software development tools
* a modern version of Erlang (as of the writing of this book, that would include versions 19 through 23); the rebar3 documentation has [great suggestions](https://www.rebar3.org/docs/getting-started#installing-erlang) on what to use here, depending upon your need
* the `rebar3` build tool for Erlang (and other BEAM languages); see [its docs](https://www.rebar3.org/docs/getting-started#installing-binary) for installation instructions
