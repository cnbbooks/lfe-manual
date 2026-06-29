# Accessing Fields: The Dot Syntax Situation

To extract values from records, LFE provides several syntactic options, ranging from the elegant to the "why would you do this to yourself."

## The Match-Extraction Method (Recommended)

Pattern matching remains the most idiomatic way to extract field values:

```lfe
(defun birthday
  (((match-person age a) p)
   (set-person-age p (+ a 1))))
```

This function pattern-matches on a person record, extracting the age field into variable `a`, while keeping the entire record bound to `p`. Then it returns a new record with the age incremented. Note the critical philosophical point: we're not changing the record (variables are immutable, remember?), we're creating a slightly modified copy—the Platonic ideal of the original, but one year older.

## The Accessor Function Method (Also Fine)

Each field gets its own accessor function automatically:

```lfe
(person-name joe)    ; returns "Joe"
(person-age joe)     ; returns 21
(person-phone joe)   ; returns "999-999"
```

These generated functions have names following the pattern `recordname-fieldname`. Predictable, if not terribly exciting.

## The Field-Pattern-Match Method (For Specific Scenarios)

You can also match against specific field values:

```lfe
(defun joes-birthday
  (((match-person name "Joe" age a) p)
   (set-person-age p (+ a 1)))
  ((p) p))  ; everyone else stays the same age
```

This only increments Joe's age. Everyone else remains ageless, which seems unfair but is presumably what they signed up for.
