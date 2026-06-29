# Nested Tuples: Turtles All the Way Down

Tuples can contain other tuples, which can contain still more tuples, ad infinitum (or at least until your computer's memory gives up in despair). This allows you to create data structures of Byzantine complexity:

```lfe
(set person
  #(person
    #(name #(first "Zaphod") #(last "Beeblebrox"))
    #(heads 2)
    #(occupation 'president)
    #(status 'froody)))
```

To extract deeply nested values, you can use nested pattern matching:

```lfe
(let ((#(person #(name #(first fname) _) _ _ _) person))
  fname)
;; Returns: "Zaphod"
```

Those who have studied ancient scrolls will recognize this as remarkably similar to destructuring in other languages, except with more parentheses and a stronger emphasis on pattern correctness.
