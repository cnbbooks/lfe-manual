# About LFE

## What LFE Isn't

Just to clear the air and set some expectations, here's what you're not going to find in LFE:

* An implementation of Scheme
* An implementation of Common Lisp
* An implementation of Clojure

As such, you will not find the following:

* A Scheme-like single namespace
* CL packages or munged names faking packages
* Access to Java libraries

## What LFE Is!

Here's what you can expect of LFE:

* A proper Lisp-2, based on the features and limitations of the Erlang VM
* Compatibility with vanilla Erlang and OTP
* It runs on the standard Erlang VM

Furthermore, as a result of Erlang's influence (and LFE's compatibility with it), the following hold:

* there is no global data
* data is not mutable
* only the standard Erlang data types are used
* you get pattern matching and guards
* you have access to Erlang functions and modules
* LFE has a compiler/interpreter
* functions with declared arity and fixed number of arguments
* Lisp macros
