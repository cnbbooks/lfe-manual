# Declaring Records: The Art of Structured Procrastination

Record declarations in LFE use the `defrecord` form, which looks refreshingly similar to how you'd explain the concept to a particularly bright parrot:

```lfe
(defrecord person
  name
  (age 0)           ; default value
  (phone ""))       ; another default
```

This declares a record type called `person` with three fields. The `age` field defaults to 0 (presumably for immortal beings or those who refuse to acknowledge the passage of time), and `phone` defaults to an empty string (for those ahead-of-their-time individuals who hadn't invented the telephone yet).

You can also write this in the more explicit form, should you feel the need to be unnecessarily verbose:

```lfe
(defrecord person
  (name 'undefined)
  (age 0)
  (phone ""))
```

The atom `'undefined` is Erlang's way of saying "I have no idea what goes here, but I'm sure it'll come to me eventually."
