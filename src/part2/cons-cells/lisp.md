# Lisp Cons Cells

In Lisp dialects, a cons cell is a fundamental data structure that holds two values or pointers to values. These two slots are traditionally called the CAR (Contents of the Address Register) and the CDR (Contents of the Decrement Register), names that derive from the original Lisp implementation on IBM 704 hardware.

The cons function constructs these memory objects, and the expression "to cons x onto y" means to construct a new cons cell with x in the car slot and y in the cdr slot.

## Structure and Notation

A simple cons cell holding two values can be represented in dotted pair notation:

```
(cons 'a 'b)  ; Creates a cons cell
=> (a . b)    ; Dotted pair notation
```

Lists in Lisp are built by having the car slot contain an element and the cdr slot point to another cons cell or to nil (the empty list). This creates a singly-linked list structure:

```
(cons 1 (cons 2 (cons 3 nil)))
=> (1 2 3)
```

## Visual Representation

Here's a diagram showing how the list `(1 2 3)` is constructed from cons cells:

```mermaid
graph LR
    A["cons cell 1"] -->|car| V1["1"]
    A -->|cdr| B["cons cell 2"]
    B -->|car| V2["2"]
    B -->|cdr| C["cons cell 3"]
    C -->|car| V3["3"]
    C -->|cdr| N["nil"]

    style A fill:#e1f5ff
    style B fill:#e1f5ff
    style C fill:#e1f5ff
    style V1 fill:#fff4e1
    style V2 fill:#fff4e1
    style V3 fill:#fff4e1
    style N fill:#ffe1e1
```

Each cons cell contains two pointers: the car points to the element value, and the cdr points to the next cons cell (or nil for the last cell).
