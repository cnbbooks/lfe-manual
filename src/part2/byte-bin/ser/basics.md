# The Basics

```lfe
;; In the REPL, observe the miracle of serialization
lfe> (set marvin-state (tuple 'robot 'depressed "brain the size of a planet"))
#(robot depressed "brain the size of a planet")

lfe> (set marvin-binary (term-to-binary marvin-state))
#B(131 104 3 100 0 5 114 111 98 111 116 100 0 9 100 101 112 114 ...)

lfe> (binary-to-term marvin-binary)
#(robot depressed "brain the size of a planet")
```

What you're witnessing here is the Erlang External Term Format (ETF), a specification that describes how to represent any Erlang/LFE term as a sequence of bytes. It's version 131, in case you were wondering, though I'm reliably informed that versions 1 through 130 were "experimental" in the sense that they involved a lot of shouting and the occasional desk-flip.
