# Nested Records: Here Be Dragons

Sometimes you need records within records, like when a person's name should itself be a structured thing:

```lfe
(defrecord name
  first
  surname)

(defrecord person
  (name (make-name first "Anonymous" surname "Coward"))
  (age 0)
  (phone ""))

; Creating a person with a structured name
(set p (make-person 
         name (make-name first "Robert" surname "Virding")))

; Accessing nested fields requires some gymnastics
(let (((match-person name n) p))
  (name-first n))  ; returns "Robert"
```

Modern Erlang (R14A+) allows chained record access, but in LFE you'll typically use pattern matching or `let` bindings to navigate nested structures. It's slightly more verbose but arguably clearer about what's happening.
