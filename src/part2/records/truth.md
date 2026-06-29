# The Terrible Truth: Records Are Just Tuples

We might as well tell you now, before you discover it yourself in a debugging session at 3 AM: records are not real. They're tuples. The compiler transforms all your lovely named-field record syntax into positional tuple operations, and the BEAM VM has no idea records ever existed.

```lfe
lfe> (set joe (make-person name "Joe" age 21 phone "999-999"))
#(person "Joe" 21 "999-999")  ; It's a tuple. It was always a tuple.
```

The first element is the record name (as an atom) acting as a "tag," and the remaining elements are the field values in the order they appeared in the record declaration. This means:

1. A `person` record is actually a 4-tuple: `#(person Name Age Phone)`
2. The shell knows about this and tries to pretty-print it as a record when possible
3. If you forget the record definition, you'll just see tuples
4. Pattern matching on records is just pattern matching on tagged tuples

**CRITICAL WARNING**: Never, under any circumstances, use the tuple representation directly in your code. Don't write `#(person "Fred" 0 "")` when you mean `(make-person name "Fred")`. The authors of this guide will disavow all knowledge of you, your code, and your existence if you do this. We have a reputation to maintain.

Why? Because using tuple representations breaks data abstraction. If you change the record definition (add a field, reorder fields), any tuple-based code will:
- Fail to compile (if you're lucky)
- Create badmatch errors (if you're moderately lucky)
- Silently do the wrong thing (if the universe hates you)

Records let you change the field order or add new fields without touching the code that uses them. Tuples don't. Choose wisely.
