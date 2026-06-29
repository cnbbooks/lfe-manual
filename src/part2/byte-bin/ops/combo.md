# Combining Operators: Real-World Example

Let's implement a simple permission system using bit flags:

```lfe
(defun perm-read () 1)      ; #b001
(defun perm-write () 2)     ; #b010
(defun perm-execute () 4)   ; #b100

(defun grant-permission (current perm)
  "Grant a permission."
  (bor current perm))

(defun revoke-permission (current perm)
  "Revoke a permission."
  (band current (bnot perm)))

(defun has-permission? (permissions perm)
  "Check if permission is granted."
  (=:= perm (band permissions perm)))

(defun describe-permissions (perms)
  "Human-readable permission description."
  (let ((r (if (has-permission? perms (perm-read)) "r" "-"))
        (w (if (has-permission? perms (perm-write)) "w" "-"))
        (x (if (has-permission? perms (perm-execute)) "x" "-")))
    (++ r w x)))
```

Testing:

```lfe
lfe> (let* ((p0 0)
            (p1 (grant-permission p0 (perm-read)))
            (p2 (grant-permission p1 (perm-write)))
            (p3 (grant-permission p2 (perm-execute)))
            (p4 (revoke-permission p3 (perm-write))))
       (list (describe-permissions p0)
             (describe-permissions p1)
             (describe-permissions p2)
             (describe-permissions p3)
             (describe-permissions p4)))
("---" "r--" "rw-" "rwx" "r-x")
```

This permission system is both space-efficient (3 bits vs. 3 booleans) and fast (bit operations are among the fastest CPU instructions).
