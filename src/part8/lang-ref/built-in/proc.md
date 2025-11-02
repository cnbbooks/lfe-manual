# Process Operations

| Function | Syntax | Description |
|----------|--------|-------------|
| `self` | `(self)` | Current process PID |
| `spawn` | `(spawn fun)` | Spawn process |
| `spawn_link` | `(spawn_link fun)` | Spawn linked process |
| `!` | `(! pid msg)` | Send message |
| `register` | `(register name pid)` | Register process name |
| `whereis` | `(whereis name)` | Look up registered process |
| `link` | `(link pid)` | Link to process |
| `unlink` | `(unlink pid)` | Unlink from process |
| `exit` | `(exit reason)` | Exit current process |
| `exit` | `(exit pid reason)` | Exit other process |

**Process Examples**:

```lisp
;; Spawn a process
(let ((pid (spawn (lambda ()
                    (receive
                      ((tuple 'msg data)
                       (io:format "Got: ~p~n" (list data))))))))
  ;; Send it a message
  (! pid (tuple 'msg "hello"))
  pid)

;; Registered processes
(register 'my-server (self))
(! 'my-server (tuple 'request 42))

;; Linked processes
(let ((pid (spawn_link (lambda () (work)))))
  ;; If pid exits, we exit too
  (monitor pid))
```
