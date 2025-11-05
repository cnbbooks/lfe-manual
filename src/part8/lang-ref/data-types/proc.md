# Process Types

| Type | Description |
|------|-------------|
| **PID** | Process identifier |
| **Port** | Port identifier (for external programs) |
| **Reference** | Unique reference |

**Process Identifiers**:

```lisp
;; Current process
(self)  ; → #Pid<0.123.0>

;; Spawn returns PID
(let ((pid (spawn (lambda () (work)))))
  (! pid 'message)
  pid)

;; Named processes
(register 'my-server (self))
(whereis 'my-server)  ; → PID
```
