# LFE Style Guide - Consolidated Reference

**Quick Reference for AI Coding Tools**

This guide consolidates LFE style conventions in order of immediate applicability: syntax → semantics → architecture.

---

## 1. SYNTAX & FORMATTING

### Indentation

**Standard: 2 spaces per form**

```lisp
(defun f ()
  (let ((x 1)
        (y 2))
    (lfe_io:format "X=~p, Y=~p~n" (list x y))))
```

**Exception: Pattern matching and conditionals**

```lisp
;; cond - align clauses
(cond ((lists:member x '(1 2 3)) "First three")
      ((=:= x 4) "Is four")
      ((>= x 5) "More than four")
      ('true "You chose poorly"))

;; defun with pattern matching - compact related clauses
(defun ackermann
  ((0 n) (+ n 1))
  ((m 0) (ackermann (- m 1) 1))
  ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))

;; Multi-line arguments
(do-something first-argument
              second-argument
              (lambda (x) (frob x))
              fourth-argument
              last-argument)
```

**Use Emacs LFE mode for consistency:**

```elisp
;; Add to ~/.emacs
(setq-default indent-tabs-mode nil)
(defvar lfe-dir (concat (getenv "HOME") "/git/lfe/emacs"))
(setq load-path (cons lfe-dir load-path))
(require 'lfe-start)
```

### Whitespace

**Vertical spacing:**
- One blank line between top-level forms
- Exception: Related simple definitions can be grouped without blank lines

```lisp
(defun +my-pi+ () 3.14)
(defun +my-e+ () 2.72)

(defun factorial (n)
  (factorial n 1))

(defun factorial
  ((0 acc) acc)
  ((n acc) (when (> n 0))
   (factorial (- n 1) (* n acc))))
```

**Horizontal spacing:**
- No extra whitespace around parentheses
- One space between forms
- All closing parens on the same line
- Do NOT vertically align middle-of-line forms

```lisp
;; BAD
(let* ((low    1)
       (high   2)
       (sum    (+ (* low low) (* high high))))
  ...)

;; GOOD
(let* ((low 1)
       (high 2)
       (sum (+ (* low low) (* high high))))
  ...)
```

### Line Length

**Maximum 80 characters per line**

Reasons:
- Web display (paste-bins, gists, documentation)
- Multiple editor panes side-by-side
- Emergency terminal access
- Encourages good naming discipline

### File Headers

```lisp
;;;; Brief description of file contents.

(defmodule module-name
  ...)
```

Copyright only needed if different from project `LICENSE` file.

---

## 2. NAMING CONVENTIONS

### Symbols (Atoms)

**Always lowercase with hyphens**

```lisp
;; BAD
*default-username*
*max-widget-cnt*

;; GOOD
*default-user-name*
*maximum-widget-count*
```

**Rules:**
- Always use `-` between words (never `/` or `.` unless well-documented reason)
- Must be pronounceable
- No overly short names except in tiny scopes

### Predicates (Boolean Functions)

**Choose ONE convention per project:**
- `trailing?` (modern Lisps) - RECOMMENDED for LFE
- `trailing-p` (multi-word classic Lisp)
- `trailingp` (single-word classic Lisp)  
- `is-leading` (Erlang style)

```lisp
;; Consistent usage - pick one style
(defun prime? (n) ...)
(defun even? (n) ...)

;; OR (not both)
(defun is-prime (n) ...)
(defun is-even (n) ...)
```

### Constants and Defaults

```lisp
;; Constants: + earmuffs +
(defun +my-pi+ () 3.14)

;; Defaults/config: * earmuffs *
(defun *default-host* () "127.0.0.1")
```

### Module-Relative Names

**Do NOT repeat module name in symbols**

```lisp
;; BAD
(defmodule varint
  (export (varint-length64 0)))

(defun varint-length64 () ...)

;; Usage becomes ugly:
(varint:varint-length64)

;; GOOD
(defmodule varint
  (export (length64 0)))

(defun length64 () ...)

;; Usage is clean:
(varint:length64)
```

### Intent Over Content

**Name by concept (intent), not implementation (content)**

```lisp
;; BAD - names reveal implementation
user-list
row-array
config-hash-table

;; GOOD - names reveal meaning
users
active-row
configuration
```

Exception: Generic algorithms operating on arbitrary data structures can use type names.

---

## 3. DOCUMENTATION

### Comment Levels

**Four levels by semicolon count:**

```lisp
;;;; File headers and large section comments

(defmodule math-n-things
  (export (utility-function 0)))

;;; Section headers for groups of related functions
;;; Can include multi-line explanatory text

(defun utility-function ()
  ;; Comment applying to following code section
  (do-something)
  (do-something-else)     ; Parenthetical remark
  (final-thing))          ; Aligned remarks
```

**Rules:**
- Always space after semicolons
- Align single-semicolon remarks vertically when consecutive

### Docstrings

**Document ALL visible functions**

```lisp
(defun small-prime-number? (n)
  "Return true if N, an integer, is a prime number. Otherwise, return false."
  ((n) (when (< n 4))
   (>= n 2))
  ((n) (when (== 0 (rem n 2)))
   'false)
  ((n)
   (lists:all #'not/1
              (lists:map (lambda (x) (== 0 (rem n x)))
                         (lists:seq 3 (trunc (math:sqrt n)))))))
```

**Format:**
- First line: concise summary
- Optional: detailed explanation after blank line
- Continuation lines indented 2 spaces (align with opening quote)
- Describe contract: inputs, outputs, side effects, conditions

**When forms don't support docstrings, use comments:**

```lisp
;;; This record tracks test results for reporting.
(defrecord state
  (status (orddict:new))
  test-type
  (ok 0)
  (fail 0))
```

### TODO and XXX Comments

```lisp
;; --- TODO (alice@gmail.com): Refactor to provide better API.

;; --- XXX (bob): Critical bug causing cascading failures.
;;                See: https://github.com/project/issues/42
```

**Conventions:**
- `TODO`: Normal tasks, incomplete features
- `XXX`: Bugs, potential issues, inelegance, uncertainty
  - Synonyms: `BUG`, `FIXME`, `HACK`
- Include identifier (username/email)
- Reference issue tracker when applicable

---

## 4. MODULE STRUCTURE

### Module Definition

**Export functions on separate lines, alphabetically sorted**

```lisp
;; NEVER use this
(defmodule maths
  (export all))  ;; NEVER DO THIS

;; BAD - wrong order, inconsistent arity grouping
(defmodule maths
  (export (factorial 2)
          (large-prime-number? 1)
          (small-prime-number? 1)
          (ackermann 2)
          (factorial 1)))

;; GOOD
(defmodule maths
  (export
   (ackermann 2)
   (factorial 1) (factorial 2)
   (large-prime-number? 1)
   (small-prime-number? 1)))

;; GOOD - separate exports for logical grouping
(defmodule maths
  (export
   (util-func 1)
   (other-util 2))
  (export
   (ackermann 2)
   (factorial 1) (factorial 2)
   (large-prime-number? 1)
   (small-prime-number? 1)))
```

### Pseudo-Packages (Sub-directories)

```lisp
;; Module in src/project/subdir/maths.lfe
(defmodule project.subdir.maths
  (export
   (factorial 1)))

;; Client usage - import for readability
(defmodule client
  (export (some-func 0))
  (import
   (from project.subdir.maths
         (factorial 1))))

(defun some-func ()
  (factorial 5))

;; OR rename to avoid collision
(defmodule client
  (export (some-func 0))
  (import
   (rename project.subdir.maths
           ((factorial 1) fact))))

(defun some-func ()
  (fact 5))
```

### When to Create Modules

**Create separate modules for:**
- Reusable functionality
- Clear separation of concerns
- Code that needs independent testing

**Workflow:**
1. Start small, stay focused
2. Write only needed functions
3. Keep functions small (one task each)
4. Make incremental changes

**For new code:**
1. Prototype in REPL
2. Paste working code into test module
3. Move final version to src module
4. Verify tests pass

---

## 5. FUNCTIONS

### Keep Functions Small

**One function = one task**

If doing six things, create six functions.

### Group Functions Logically

```lisp
;;;; Exported Functions

(defun public-api-1 () ...)
(defun public-api-2 () ...)

;;;; Internal Functions

(defun helper-1 () ...)
(defun helper-2 () ...)
```

Generally put exported functions first, unless readability benefits from different ordering.

### Refactor Complex Conditionals

**Extract complex conditions into named predicates**

```lisp
;; BAD - inline complexity
(if (and (fuelled? rocket)
         (lists:all #'strapped-in? (crew rocket))
         (sensors-working? rocket))
  (launch rocket)
  (! pid `#(err "Aborting launch.")))

;; GOOD - named abstraction
(defun rocket-ready? (rocket)
  (and (fuelled? rocket)
       (lists:all #'strapped-in? (crew rocket))
       (sensors-working? rocket)))

(if (rocket-ready? rocket)
  (launch rocket)
  (! pid `#(err "Aborting launch.")))
```

### Pattern Match in Function Heads

**Don't write complex case statements with deep nesting**

Split into functions using pattern matching in heads.

---

## 6. DATA STRUCTURES

### Lists

**Appropriate access patterns:**

```lisp
;; Simple access
(car lst)
(cdr lst)
(cadr lst)

;; Pattern matching
(let ((`(,head . ,tail) lst))
  ...)

;; Erlang lists module
(lists:nth 1 lst)
(lists:reverse lst)
```

**Avoid using lists as ad-hoc structures**
- Don't use lists to pass multiple heterogeneous values
- Exception: function argument lists, apply arguments
- Use records for heterogeneous collections

### Maps

**Do NOT align keys and values**

```lisp
;; BAD
'#m(k1            v1
    key2          value2
    key-the-third value-the-third)

;; GOOD
#m(k1 v1
   key2 value2
   key-the-third value-the-third
   another one)
```

### Records

**Primary data structure for messages and complex data**

```lisp
;; Define in header (.lfe) if used across modules
;; Define in module if used only locally

(defrecord person
  name
  age
  occupation)

;; ALWAYS use record macros - NEVER match tuples directly
;; BAD
(let ((`#(person ,name ,age) joe))
  ...)

;; GOOD
(let* ((joe (make-person name "Joe" age 29))
       (name (person-name joe)))
  ...)
```

**Benefits:**
- Cross-module consistency
- Enforced by compiler
- Self-documenting code

### Tuples and Proplists

**Standard formatting (no alignment)**

```lisp
;; BAD
'(#(k1            v1)
  #(key2          value2)
  #(key-the-third value-the-third))

;; GOOD
'(#(k1 v1)
  #(key2 value2)
  #(key-the-third value-the-third)
  #(another one))
```

---

## 7. PROCESSES AND CONCURRENCY

### One Process Per Module

**Code for a process's top loop stays in ONE module**

- Process can call library functions elsewhere
- But control flow must be in single module
- Conversely: one module = one process type

### Process-to-Activity Mapping

**One parallel process per truly concurrent real-world activity**

This creates natural, understandable structure.

### Process Roles

**Each process has ONE role:**
- Client
- Server  
- Supervisor (watches/restarts others)
- Worker (can have errors)
- Trusted Worker (must not error)

Don't mix roles in one process.

### Registered Processes

**Register with same name as module**

```lisp
(defmodule fileserver
  ...)

;; Register as 'fileserver
(register 'fileserver pid)
```

Only register long-lived processes.

### Process Dictionary - Avoid

**Do NOT use get/put unless absolutely necessary**

```lisp
;; BAD - uses process dictionary
(defun tokenize
  ((`(,head . ,tail)) ...)
  (('())
   (case (get-characters-from-device (get 'device))
     ('eof '())
     (`#(value ,chars) (tokenize chars)))))

;; GOOD - explicit parameter
(defun tokenize
  ((device `(,head . ,tail)) ...)
  ((device '())
   (case (get-characters-from-device device)
     ('eof '())
     (`#(value ,chars) (tokenize device chars)))))
```

Process dictionary makes functions non-deterministic and harder to debug.

---

## 8. SERVERS

### Use Generic Servers

**Leverage OTP gen_server or similar libraries**

Consistent use of generic servers simplifies system architecture.

### Tail Recursion Required

**ALL servers must be tail-recursive**

```lisp
;; BAD - not tail recursive, will consume memory
(defun loop ()
  (receive
    (`#(msg1 ,msg1)
     ...
     (loop))
    ('stop 'true))
  (io:format "Server going down" '()))  ;; DON'T DO THIS

;; GOOD - tail recursive
(defun loop ()
  (receive
    (`#(msg1 ,msg1)
     ...
     (loop))
    ('stop
     (io:format "Server going down" '()))
    (other
     (logger:error "Unknown msg ~w~n" `(,other))
     (loop))))
```

---

## 9. MESSAGES

### Tag All Messages

**Makes receive order irrelevant and extension easier**

```lisp
;; BAD - untagged messages
(defun loop (state)
  (receive
    (`#(,mod ,funcs ,args)  ;; Ambiguous!
     (erlang:apply mod funcs args)
     (loop state))))

;; GOOD - tagged messages
(defun loop (state)
  (receive
    (`#(execute ,mod ,funcs ,args)
     (erlang:apply mod funcs args)
     (loop state))
    (`#(get_status_info ,from ,option)
     (! from `#(status_info ,(get-status-info option state)))
     (loop state))))
```

### Use Tagged Return Values

```lisp
;; BAD - can't distinguish false result from not-found
(defun keysearch
  ((key `(#(,key ,value) . ,tail)) value)
  ((key `(#(,_ ,_) . ,tail)) (keysearch key tail))
  ((key '()) 'false))

;; GOOD - tagged return
(defun keysearch
  ((key `(#(,key ,value) . ,tail)) `#(value ,value))
  ((key `(#(,_ ,_) . ,tail)) (keysearch key tail))
  ((key '()) 'false))
```

### Flush Unknown Messages

**Always have catch-all in receive**

```lisp
(defun main-loop ()
  (receive
    (`#(msg1 ,msg1)
     ...
     (main-loop))
    (`#(msg2 ,msg2)
     ...
     (main-loop))
    (other
     (logger:error "Process ~w got unknown msg ~w~n" 
                   `(,(self) ,other))
     (main-loop))))
```

### Use Interface Functions

**Encapsulate message passing behind functions**

```lisp
(defmodule fileserver
  (export
   (start 0)
   (stop 0)
   (open-file 1)))

(defun open-file (filename)
  (! fileserver `#(open-file-request ,filename))
  (receive
    (`#(open-file-response ,result) result)))
```

Message protocol is internal, hide it from clients.

### Timeouts

**Be careful with after in receive**

Must handle case where message arrives late.

### Exit Trapping

**Minimize processes that trap exits**

Processes should either always trap or never trap. Don't toggle.

---

## 10. ERROR HANDLING

### Separate Error and Normal Case

**Don't clutter normal case with error handling**

```lisp
;; Program for the happy path
;; Let it crash on errors
;; Handle errors in separate supervisor process
```

Clean separation simplifies system design.

### Identify Error Kernel

**Determine what MUST be correct**

Like OS kernel vs user programs:
- Error kernel must be correct
- Application code can fail without system compromise
- Often contains critical real-time state

---

## 11. LIBRARIES AND PROJECTS

### Use Before You Write

**Search for existing libraries first**

- No-dependency projects aren't virtuous
- Doesn't aid portability or executable creation
- Check licensing compatibility

### When to Write Alternative Library

**Only if you provide significant value:**
- Complete documentation
- Comprehensive examples
- Well-designed website/resources

### Structure Projects as Library Collections

**Many small libraries > one monolith**

If you abandon 30% of a monolith, it's useless to others.
If you abandon a collection of libraries, you've left useful components.

### Project Structure with Pseudo-Packages

```text
├── LICENSE
├── README.md
├── rebar.config
├── src
│   ├── title.app.src
│   └── title
│       ├── config.lfe
│       ├── db.lfe
│       ├── graphics
│       │   ├── mesh.lfe
│       │   ├── obj.lfe
│       │   └── gl.lfe
│       └── logging.lfe
└── test
```

Compiles to flat structure:
```text
ebin/
  title.app
  title.config.beam
  title.db.beam
  title.graphics.mesh.beam
  title.graphics.obj.beam
  title.graphics.gl.beam
  title.logging.beam
```

---

## 12. GENERAL PRINCIPLES

### Team Development

**Every developer must remember:**

1. **"Hit by a truck" principle**: Others must understand your code
2. **Consistency**: All code should look the same
3. **Precision**: Be exact
4. **Conciseness**: Say more with less
5. **Simplicity**: Use smallest hammer for job
6. **Common sense**: Think before coding
7. **Cohesion**: Keep related code together

### Priority Order

**When making decisions, optimize in this order:**

1. Usability by customer
2. Debuggability/Testability
3. Readability/Comprehensibility
4. Extensibility/Modifiability
5. Efficiency (runtime performance)

### Efficiency Notes

- Choose performant option when complexity is equal
- Choose simpler option when complexity differs
- Profile before optimizing
- Avoid premature optimization
- Don't optimize rarely-run code

### Architecture

**For non-trivial changes:**
- Write design document (at least 2 paragraphs)
- Get approval from affected parties
- Consider: reusability, component communication, evolution, team coordination

---

## QUICK SYNTAX CHECKLIST

**Before committing LFE code, verify:**

- [ ] 2-space indentation (use Emacs LFE mode)
- [ ] No lines > 80 characters
- [ ] Lowercase atoms with hyphens (not underscores)
- [ ] Module names match filenames
- [ ] Exported functions listed alphabetically
- [ ] Docstrings on all public functions
- [ ] Comments use appropriate semicolon count
- [ ] No (export all)
- [ ] Records used for structured data
- [ ] Pattern matching in function heads, not case
- [ ] Tail-recursive server loops
- [ ] Tagged messages
- [ ] Interface functions hiding message protocols
- [ ] Unknown messages flushed in receive
- [ ] Complex conditionals extracted to named predicates

---

## SPELLING CONVENTIONS

**Correct spellings:**
- complimentary (free meal) not complementary
- existent, nonexistent, existence (not -ant/-ance)
- hierarchy (not heirarchy)
- precede (not preceed)
- weird (not wierd)

**Exceptions (industry standard):**
- referer (HTTP header, not referrer)

**Use aspell or similar for spell-checking.**

---

## SOURCES

This guide synthesizes conventions from:
- Google Common Lisp Style Guide
- Common Lisp Style Guide (lisp-lang.org)
- Peter Norvig & Kent Pitman's Lisp Style Tutorial
- Erlang Programming Rules and Conventions
- Inaka Erlang Guidelines
- Clojure Style Guide
- James Halliday's module philosophy

**Remember**: These are guidelines, not laws. Consistency within a project matters most. When contributing to existing projects, match their established style.

---

*End of Consolidated LFE Style Guide*
