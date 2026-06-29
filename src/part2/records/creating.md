# Creating Records: Instantiation Without the Existential Crisis

Once you've declared a record, creating instances is pleasantly straightforward:

```lfe
; Creating a new person with all fields specified
(make-person name "Joe" age 21 phone "999-999")

; Creating a person and letting defaults do their thing
(make-person name "Fred")  ; age=0, phone=""

; The order doesn't matter (take that, positional parameters!)
(make-person phone "999-999" name "Joe" age 21)
```

The `make-person` function is generated automatically by the `defrecord` declaration. It's one of those delightful compiler conveniences that makes you wonder why you ever bothered with manual tuple construction in the first place.

Note that you can specify fields in any order you fancy. The compiler will sort it out, much like a particularly organized filing clerk who insists on alphabetizing everything despite your protests that chaos is more authentic.
