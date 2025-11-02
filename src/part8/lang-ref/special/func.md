# Functions and Closures

| Form | Syntax | Description |
|------|--------|-------------|
| `lambda` | `(lambda (args) body)` | Anonymous function |
| `match-lambda` | `(match-lambda (pattern body) ...)` | Pattern-matching function |
| `let` | `(let ((var val) ...) body)` | Parallel bindings |
| `let*` | `(let* ((var val) ...) body)` | Sequential bindings |
| `letrec` | `(letrec ((var val) ...) body)` | Recursive bindings |
| `let-function` | `(let-function ((name lambda) ...) body)` | Local functions |
| `letrec-function` | `(letrec-function ((name lambda) ...) body)` | Recursive local functions |
| `let-macro` | `(let-macro ((name macro) ...) body)` | Local macros |

**Lambda Examples**:

```lisp
;; Simple lambda
(lambda (x) (* x x))

;; Multi-argument
(lambda (x y) (+ x y))

;; Zero arguments
(lambda () 42)

;; Variable-arity (not directly supported - use match-lambda)
```

**Match-Lambda Examples**:

```lisp
;; Pattern matching on arguments
(match-lambda
  ([0] 1)                          ; Base case
  ([n] (when (> n 0))              ; Guard
   (* n (factorial (- n 1)))))     ; Recursive case

;; Multiple patterns
(match-lambda
  ([(cons h t)] (list 'cons h t))  ; List pattern
  ([x] (when (is_atom x)) 'atom)   ; Guard pattern
  ([_] 'other))                     ; Catch-all
```

**Let Forms Comparison**:

```lisp
;; let - parallel binding (x, y unbound during evaluation)
(let ((x 1)
      (y 2))
  (+ x y))  ; → 3

;; let* - sequential binding (x bound during y evaluation)
(let* ((x 1)
       (y (+ x 1)))
  (+ x y))  ; → 3

;; letrec - recursive binding (both bound during evaluation)
(letrec ((even? (lambda (n)
                  (if (== n 0) 'true (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (== n 0) 'false (even? (- n 1))))))
  (even? 4))  ; → true
```

**Function Binding Examples**:

```lisp
;; let-function - local function definitions
(let-function ((square (lambda (x) (* x x)))
               (cube (lambda (x) (* x x x))))
  (+ (square 2) (cube 3)))  ; → 31

;; letrec-function - mutually recursive functions
(letrec-function ((even? (lambda (n)
                           (if (== n 0) 'true (odd? (- n 1)))))
                  (odd? (lambda (n)
                          (if (== n 0) 'false (even? (- n 1))))))
  (even? 10))  ; → true
```
